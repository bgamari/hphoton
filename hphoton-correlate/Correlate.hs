{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative
import           Control.Error
import           Control.Monad           (forM_, liftM, when)
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
                 , jiffy_   :: Maybe RealTime
                 , shortlag :: RealTime
                 , longlag  :: RealTime
                 , nlags    :: Int
                 , verbose  :: Bool
                 }
            deriving (Show,Eq)

opts :: Parser Args
opts = Args
    <$> strOption ( help "File containing timestamps"
                 <> short 'x'
                  )
    <*> option    auto
                  ( help "Channel"
                 <> short 'X'
                 <> value 0
                  )
    <*> option    (Just <$> auto)
                  ( help "File containing timestamps"
                 <> value Nothing
                 <> short 'y'
                  )
    <*> option    auto
                  ( help "Channel"
                 <> short 'Y'
                 <> value 0
                  )
    <*> option    (Just <$> auto)
                  ( help "Override timestamp timebase period"
                 <> long "jiffy"
                 <> short 'j'
                 <> value Nothing
                 <> metavar "TIME"
                  )
    <*> option    auto
                  ( help "Minimum lag to compute"
                 <> long "min-lag"
                 <> short 'l'
                 <> value 1e-6
                 <> metavar "TIME"
                  )
    <*> option    auto
                  ( help "Maximum lag to computer"
                 <> long "max-lag"
                 <> short 'L'
                 <> value 1
                 <> metavar "TIME"
                  )
    <*> option    auto
                  ( help "Number of lags to compute per octave"
                 <> long "nbins"
                 <> short 'n'
                 <> value 20
                  )
    <*> switch    ( help "Enable verbose output"
                 <> short 'v'
                 <> long "verbose"
                  )

description = intercalate "\n"
    [ "Corr efficiently computes correlation functions for single-dimensional"
    , "discrete-time, binary data (e.g. photon counts)."
    , ""
    , "It takes as input two files of timestamps (in a number of supported formats)"
    , "and produces an ASCII file containing an estimate of the correlation function"
    , "and its variance over the requested range of lag times."
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

    jiffy' <- case (jiffy_ args, lookupMetadata _Jiffy metaA, lookupMetadata _Jiffy metaB) of
      (Just j, _, _)           -> return j
      (_ , Nothing, Nothing)   -> left "Could't infer jiffy. Specify manually with --jiffy"
      (_ , Just ja, Nothing)   -> right ja
      (_ , Nothing, Just jb)   -> right jb
      (_ , Just ja, Just jb)
        | ja /= jb  -> left "Incompatible jiffys"
        | otherwise -> right ja

    checkMonotonic a
    checkMonotonic b
    when (verbose args) $ do
        liftIO $ putStrLn $ "x: "++show (V.length a)++" timestamps"
        liftIO $ putStrLn $ "y: "++show (V.length b)++" timestamps"

    let clk = clockFromJiffy jiffy'
        expDur = duration [a,b]
    when (10 * realTimeToTime clk (longlag args) > expDur)
      $ left "--max-lag is too long for data set"
    when (realTimeToTime clk (shortlag args) < 10)
      $ left "--min-lag is too short for data set"


    let pts = let short = realTimeToTime clk (shortlag args)
                  long = realTimeToTime clk (longlag args)
                  maybeError = fromMaybe (error "Empty vector")
              in withStrategy (parList rdeepseq)
                 $ logCorr (short, long) (nlags args)
                           (maybeError $ unsafeVecFromStamps a)
                           (maybeError $ unsafeVecFromStamps b)

    liftIO $ forM_ pts $ \(lag, gee, bar) -> do
        printf "%1.4e\t%1.8f\t%1.8e\n" (timeToRealTime clk lag) gee (bar^2)
        hFlush stdout

