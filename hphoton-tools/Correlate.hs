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

import           HPhoton.Corr.PackedVec  (PackedVec (PVec))
import           HPhoton.Corr.SparseCorr
import           HPhoton.IO
import           HPhoton.Types

import           Options.Applicative
import           System.IO
import           Text.Printf
import           Debug.Trace

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
    (b,metaB) <- fmapLT show $ readStamps (fromMaybe (xfile args) (yfile args)) (ychan args)
    checkMonotonic a
    checkMonotonic b

    let clk = clockFromJiffy $ jiffy_ args
    let pts = withStrategy (parList rdeepseq)
              $ logCorr clk (shortlag args, longlag args) (nlags args)
                        (vecFromStamps' a) (vecFromStamps' b)

    liftIO $ forM_ pts $ \(lag, gee, bar) -> do
        printf "%1.4e\t%1.8f\t%1.8e\n" lag gee bar
        hFlush stdout

logCorr :: V.Vector v (Time, Int)
        => Clock -> (RealTime, RealTime) -> Int
        -> BinnedVec v Time Int -> BinnedVec v Time Int -> [(RealTime, Double, Double)]
logCorr clk (minLag, maxLag) lagsPerOctave a b =
    let nOctaves = ceiling $ log2 maxLag - log2 minLag
        m = 2**(1 / realToFrac lagsPerOctave)
        lags = map (realTimeToTime clk) 
               $ [minLag * m^i | i <- [0..lagsPerOctave*nOctaves-1]]
        binResizes = take (nOctaves * lagsPerOctave)
                   $ replicate (2*lagsPerOctave) 1
                  ++ cycle (take lagsPerOctave $ 2:repeat 1)
        f :: V.Vector v (Time, Int)
          => [Int] -> Int -- ^ Measured in bins
          -> BinnedVec v Time Int -> BinnedVec v Time Int
          -> [(RealTime, Double, Double)]
        f [] _ _ _ = []
        f (binSz:rest) lag a b =
            let width = binnedWidth a
                lag' = fromIntegral lag * width
                (gee, bar) = corr (realTimeToTime clk maxLag) a b lag'
                realLag = realToFrac $ timeToRealTime clk lag'
            in traceShow (width, lag, lag', realLag)
               (realLag, gee, bar) : f rest ((lag+1) `div` binSz) (rebin binSz a) (rebin binSz b)
    in f binResizes 1 a b

