module Main (main) where

import           Control.Applicative
import           Control.Monad           (forM_, liftM)
import qualified Data.Binary.Get         as B
import qualified Data.ByteString.Lazy    as BS
import           Data.List
import           Data.Monoid
import qualified Data.Vector.Unboxed     as V
import           Data.Word

import           HPhoton.Corr.PackedVec  (PackedVec (PVec))
import           HPhoton.Corr.SparseCorr
import           HPhoton.RawTimestamps
import           HPhoton.Types

import           Options.Applicative
import           System.IO
import           Text.Printf

type Stamps = V.Vector Time

log10 = logBase 10
linspace, logspace :: Int -> (Double, Double) -> [Double]
linspace n (a,b) = [a + fromIntegral i/fromIntegral n*(b-a) | i <- [1..n]]
logspace n (a,b) = map (10**) $ linspace n (a,b)

data Args = Args { xfile    :: String
                 , yfile    :: String
                 , jiffy_   :: RealTime
                 , shortlag :: RealTime
                 , longlag  :: RealTime
                 , nlags    :: Int }
            deriving (Show,Eq)

opts = Args
    <$> strOption ( help "File containing timestamps"
                 <> short 'x'
                  )
    <*> strOption ( help "File containing timestamps"
                 <> short 'y'
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
    <*> option    ( help "Number of lags to computer per decade"
                 <> long "nbins"
                 <> short 'n'
                 <> value 300
                  )
description = intercalate "\n"
    [ "Corr efficiently computes correlation functions for single-dimensional"
    , "discrete-time, binary data (e.g. photon counts)."
    , ""
    , "It takes as input two files of timestamps (unsigned 64-bit binary integers)"
    , "and produces an ASCII file containing an estimate of the correlation function"
    , "and its variance for the requested range of lag times."
    ]

checkMonotonic :: Stamps -> IO ()
checkMonotonic v =
    let f (l,t) t' | t > t'      = (t:l,t')
                   | otherwise   = (l,t')
    in case V.foldl' f ([],0) v of
            ([],_) -> return ()
            (l,_)  -> print $ "Non-monotonic:" ++ show l

main :: IO ()
main = do
    args <- execParser $ info (helper <*> opts)
        ( fullDesc
       <> header "Compute the correlation function of binary, discrete-time data"
       <> progDesc description
        )

    let f = V.drop 1024 . V.convert -- HACK
    a <- f <$> readStamps (xfile args)
    b <- f <$> readStamps (yfile args)
    checkMonotonic a
    checkMonotonic b

    let clk = clockFromJiffy $ jiffy_ args
    let pts = logCorr clk (shortlag args, longlag args) (nlags args)
                      (vecFromStamps a) (vecFromStamps b)

    --let lags = linspace nlags (shortlag args, longlag args)
    --let lags = logspace (nlags args) (log10 (shortlag args), log10 (longlag args))
    --let pts = computeSplitCorr (round $ 1e-3 / jiffy args) (jiffy args) lags a b
    --let pts = computeCorr clk lags (packedVecFromStamps a) (packedVecFromStamps b)

    forM_ pts $ \(lag, gee, bar) -> do
        printf "%1.4e\t%1.8f\t%1.8e\n" lag gee bar
        hFlush stdout

logCorr :: Clock -> (RealTime, RealTime) -> Int
        -> BinnedVec Time Int -> BinnedVec Time Int -> [(RealTime, Double, Double)]
logCorr clk (minLag, maxLag) lagsPerDecade a b =
    let nDecades = round $ log10 maxLag - log10 minLag
        lags = logspace (nDecades*lagsPerDecade) (log10 minLag, log10 maxLag)
        f [] _ _ = []
        f ((lag,binSz):rest) a b =
            let a' = rebin binSz a
                b' = rebin binSz b
                width = binnedWidth a'
                lag' = realTimeToTime clk lag `quot` width * width
                (gee, bar) = corr (realTimeToTime clk maxLag) a' b' lag' --(realTimeToTime clk lag)
            in (lag, gee, bar) : f rest a' b'
    in f (zip lags (cycle $ replicate (lagsPerDecade-1) 1 ++ [10])) a b

