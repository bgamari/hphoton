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
import           System.Exit

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
                 <> metavar "FILE"
                  )
    <*> option    auto
                  ( help "Channel"
                 <> short 'X'
                 <> metavar "CHANNEL"
                 <> value 0
                  )
    <*> option    (Just <$> str)
                  ( help "File containing timestamps"
                 <> value Nothing
                 <> short 'y'
                 <> metavar "FILE"
                  )
    <*> option    auto
                  ( help "Channel"
                 <> short 'Y'
                 <> metavar "CHANNEL"
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
                 <> metavar "N"
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

checkMonotonic :: Monad m => Stamps -> ExceptT String m ()
checkMonotonic v =
    let f (l,t) t' | t > t'      = (t:l,t')
                   | otherwise   = (l,t')
    in case V.foldl' f ([],0) v of
            ([],_) -> return ()
            (l,_)  -> throwE $ "Non-monotonic:" ++ show l

main = do
    result <- runExceptT main'
    case result of
      Left err  -> do
        hPutStrLn stderr $ "Error: "++err
        exitWith $ ExitFailure 1
      Right _   -> return ()

main' :: ExceptT String IO ()
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
      (_ , Nothing, Nothing)   -> throwE "Could't infer jiffy. Specify manually with --jiffy"
      (_ , Just ja, Nothing)   -> return ja
      (_ , Nothing, Just jb)   -> return jb
      (_ , Just ja, Just jb)
        | ja /= jb  -> throwE "Incompatible jiffys"
        | otherwise -> return ja

    checkMonotonic a
    checkMonotonic b
    when (verbose args) $ do
        liftIO $ putStrLn $ "x: "++show (V.length a)++" timestamps"
        liftIO $ putStrLn $ "y: "++show (V.length b)++" timestamps"

    let clk = clockFromJiffy jiffy'
        expDur = duration [a,b]
    when (10 * realTimeToTime clk (longlag args) > expDur)
      $ throwE "--max-lag is too long for data set"
    when (realTimeToTime clk (shortlag args) < 10)
      $ throwE "--min-lag is too short for data set"


    let pts = let short = realTimeToTime clk (shortlag args)
                  long = realTimeToTime clk (longlag args)
              in withStrategy (parList rseq)
                 $ filter (\(Point lag _ _) -> lag > short)
                 $ logCorr long (nlags args)
                           (unsafeVecFromStamps a)
                           (unsafeVecFromStamps b)

    liftIO $ forM_ pts $ \(Point lag gee bar) -> do
        printf "%1.4e\t%1.8f\t%1.8e\n" (timeToRealTime clk lag) gee (bar^2)
        hFlush stdout

