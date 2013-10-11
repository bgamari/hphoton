{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative
import           Control.Error
import           Control.Monad           (forM_, liftM)
import           Control.Monad.Trans
import           Data.List
import           Data.Function (on)
import           Data.Monoid
import qualified Data.Vector.Generic     as V
import qualified Data.Vector.Unboxed     as VU
import           Data.Word
import           Data.Maybe (fromMaybe)
import           Control.Parallel.Strategies                 

import           HPhoton.Corr.PackedVec  (PackedVec)
import           HPhoton.Corr.SparseCorr
import           HPhoton.IO
import           HPhoton.Types

import           Options.Applicative
import           System.IO
import           Text.Printf

type Stamps = VU.Vector Time

log2 = logBase 2

data Args = Args { xfile    :: FilePath
                 , xchan    :: Channel
                 , yfile    :: Maybe FilePath
                 , ychan    :: Channel
                 , jiffy_   :: RealTime
                 , shortlag :: RealTime
                 , longlag  :: RealTime
                 , nlags    :: Int }
            deriving (Show,Eq)

opts :: Parser Args
opts = Args
    <$> strOption ( help "File containing timestamps"
                 <> short 'x'
                  )
    <*> option    ( help "Channel"
                 <> short 'X'
                 <> value 0
                  )
    <*> option    ( help "File containing timestamps"
                 <> value Nothing
                 <> reader (pure . str)
                 <> short 'y'
                  )
    <*> option    ( help "Channel"
                 <> short 'Y'
                 <> value 0
                  )
    <*> option    ( help "Timestamp timebase period"
                 <> long "jiffy"
                 <> short 'j'
                 <> value (1/4e12)
                 <> metavar "TIME"
                  )
    <*> option    ( help "Minimum lag to compute"
                 <> long "min-lag"
                 <> short 'l'
                 <> value 1e-6
                 <> metavar "TIME"
                  )
    <*> option    ( help "Maximum lag to computer"
                 <> long "max-lag"
                 <> short 'L'
                 <> value 1
                 <> metavar "TIME"
                  )
    <*> option    ( help "Number of lags to compute per octave"
                 <> long "nbins"
                 <> short 'n'
                 <> value 20
                  )
description = intercalate "\n"
    [ "Corr efficiently computes correlation functions for single-dimensional"
    , "discrete-time, binary data (e.g. photon counts)."
    , ""
    , "It takes as input two files of timestamps (unsigned 64-bit binary integers)"
    , "and produces an ASCII file containing an estimate of the correlation function"
    , "and its variance for the requested range of lag times."
    ]

checkMonotonic :: Monad m => Stamps -> EitherT String m ()
checkMonotonic v =
    let f (l,t) t' | t > t'      = (t:l,t')
                   | otherwise   = (l,t')
    in case V.foldl' f ([],0) v of
            ([],_) -> return ()
            (l,_)  -> left $ "Non-monotonic:" ++ show l

main = do
    result <- runEitherT main'
    case result of
      Left err  -> putStrLn $ "Error: "++err
      Right _   -> return ()
      
main' :: EitherT String IO ()
main' = do
    args <- lift $ execParser $ info (helper <*> opts)
        ( fullDesc
       <> header "Compute the correlation function of binary, discrete-time data"
       <> progDesc description
        )

    (a,metaA) <- fmapLT show $ readStamps (xfile args) (xchan args)
    (b,metaB) <- case yfile args of
                   Nothing
                     | xchan args == ychan args -> return (a,metaA)
                     | otherwise                -> fmapLT show $ readStamps (xfile args) (xchan args)
                   Just f  -> fmapLT show $ readStamps f (ychan args)
    checkMonotonic a
    checkMonotonic b

    let clk = clockFromJiffy $ jiffy_ args
    let pts = let short = realTimeToTime clk (shortlag args)
                  long = realTimeToTime clk (longlag args)
              in withStrategy (parList rdeepseq)
                 $ logCorr (short, long) (nlags args)
                           (unsafeVecFromStamps a) (unsafeVecFromStamps b)

    liftIO $ forM_ pts $ \(lag, gee, bar) -> do
        printf "%1.4e\t%1.8f\t%1.8e\n" (timeToRealTime clk lag) gee (bar^2)
        hFlush stdout
