{-# LANGUAGE PackageImports, TupleSections #-}

module Main (main) where

import Control.Monad
import Data.List
import Data.Vector.Unboxed ((!), Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BS
import Data.Word
import System.Environment
import System.IO
import Text.Printf

import Data.Random
import qualified System.Random.MWC as MWC
import Data.Random.Distribution.Exponential
import Bin

import Graphics.Rendering.Chart
import "data-accessor" Data.Accessor
import Debug.Trace

type Time = Int
type RealTime = Double
type Prob = Double

jiffy = 1/128e6 :: RealTime
--jiffy = 10000 :: RealTime -- | 1/clockrate

realRateToTau rate = round $ 1/(rate*jiffy)
testTauBG = realRateToTau 20
modelTauBG = realRateToTau 700
testTauBurst = realRateToTau 60
modelTauBurst = realRateToTau 2500

-- | LogP: Represents a log probability
data LogP a = LogP a deriving (Eq, Ord, Show)

instance (Real a, Floating a) => Num (LogP a) where
        LogP x + LogP y = let x' = min x y
                              y' = max x y
                          in  LogP (y' + log (1 + exp (x'-y')))
        LogP x * LogP y = LogP (x + y)
        abs (LogP x)    = LogP x
        signum (LogP x) = 1
        fromInteger x   = LogP (log (fromInteger x))

instance (Real a, Floating a) => Fractional (LogP a) where
        LogP x / LogP y = LogP (x - y)
        fromRational x  = LogP (log (fromRational x))

instance (Real a, Floating a) => Real (LogP a) where
        toRational (LogP x) = toRational $ exp x

-- | Output the value x along with a message
trace1 msg x = trace (msg ++ " " ++ show x) x

-- | Encapsulates the parameters of the burst model
data ModelParams = ModelParams { prob_b :: Prob
                               , tau_burst :: Time
                               , tau_bg :: Time }
                               deriving (Show)
prob_nb = (1-) . prob_b

-- | Default model parameters
def_mp = ModelParams { prob_b = 0.05
                     , tau_bg = modelTauBG
                     , tau_burst = modelTauBurst
                     }

-- | The PDF of an exponential distribution with inverse rate tau
exp_pdf :: Double -> Double -> Prob
exp_pdf tau t = 1/tau * exp(-t/tau)

prob_dt__b, prob_dt__nb :: ModelParams -> Time -> Prob
-- | P(dt | Burst)
prob_dt__b mp dt = 1/tau * exp(-realToFrac dt / tau) -- TODO: There should be a distribution over tau_burst?
                   where tau = realToFrac $ tau_burst mp
-- | P(dt | !Burst)
prob_dt__nb mp = exp_pdf (realToFrac $ tau_bg mp) . realToFrac

-- | Compute the Bayes factor beta_n for a given time t
beta :: Int -> V.Vector Time -> ModelParams -> Time -> Double
beta n dts mp t
        | t+n >= V.length dts = error "Out of range"
        | otherwise = let prob_b__dts = prob_b mp * (product $ map (\i->prob_dt__b mp (dts!(i+t))) [0..n])
                          prob_nb__dts = prob_nb mp * (product $ map (\i->prob_dt__nb mp (dts!(i+t))) [0..n])
                      in prob_b__dts / prob_nb__dts

-- | Find bursts
findBursts :: Int -> V.Vector Time -> ModelParams -> [Time]
findBursts n dts mp = let accept t = beta n dts mp t > 2 -- TODO: Why is this so small?
                    in filter accept [0..(V.length dts - n)]

data CompressSpansState = CSpansState { startT :: Time
                                      , lastT  :: Time
                                      , conseqs :: [(Time,Time)]
                                      } deriving Show

-- | Reduce a list of times to a list of (startTime, endTime) spans
compressSpans :: [Time] -> [(Time, Time)]
compressSpans [] = error "Can't compress an empty list"
compressSpans ts = let f :: CompressSpansState -> Time -> CompressSpansState
                       f s t
                                | t == lastT s = error $ "compressSpans: Uh oh! dt=0 at " ++ show t
                                | t == lastT s + 1 = s {lastT=t}
                                | otherwise = s {startT=t, lastT=t, conseqs=(startT s, lastT s):conseqs s}
                       s = CSpansState {startT = -1, lastT = -1, conseqs = []}
                       CSpansState _ _ compressed = foldl' f s ts
                   in if null compressed then error "No spans found"
                                         else tail $ reverse compressed 

-- | Reduce a list of times to a list of (startTime, endTime) spans with fuzz
compressFuzzySpans :: [Time] -> Time -> [(Time, Time)]
compressFuzzySpans [] _ = error "Can't compress an empty list"
compressFuzzySpans ts fuzz = let f :: CompressSpansState -> Time -> CompressSpansState
                                 f s t
                                          | t == lastT s = error $ "compressSpans: Uh oh! dt=0 at " ++ show t
                                          | t - lastT s <= fuzz = s {lastT=t}
                                          | otherwise = s {startT=t, lastT=t, conseqs=(startT s, lastT s):conseqs s}
                                 s = CSpansState {startT = -1, lastT = -1, conseqs = []}
                                 CSpansState _ _ compressed = foldl' f s ts
                             in if null compressed then error "No spans found"
                                                   else tail $ reverse compressed 

-- | A perfectly periodic set of inter-arrival times
testData :: [Time]
testData = let v = (replicate 900 testTauBG) ++ (replicate 100 testTauBurst)
           in take 100000 $ cycle v

-- | Produce realistic inter-arrival times
testData2 :: IO [Time]
testData2 = do mwc <- MWC.create
               let sample :: Int -> Double -> IO [Double]
                   sample n tau = replicateM n $ sampleFrom mwc $ exponential tau
                   sCycle :: IO [Double]
                   sCycle = liftM join $ sequence [sample 900 $ realToFrac testTauBG, sample 100 $ realToFrac testTauBurst]
               a <- replicateM 100 sCycle
               return $ map ceiling $ join a

mean, stdev :: (RealFrac a) => [a] -> a
mean v = sum v / (realToFrac $ length v)
stdev v = let m = mean v
              ss = sum $ map (\x->(m-x)^2) v
          in ss / (realToFrac $ length v)

guessTaus :: V.Vector Time -> (Time, Time)
guessTaus dts = let width = round (1e-3/jiffy) :: Time
                    bins = binTimes (V.toList $ V.scanl1' (+) dts) width
                    rbins = map realToFrac bins :: [Double]
                    thresh = mean rbins + 1*stdev rbins
                    bg_tau = realToFrac width / (mean $ filter (<thresh) rbins)
                    burst_tau = realToFrac width / (mean $ filter (>thresh) rbins)
                in (round bg_tau, round burst_tau)


-- | Read 64-bit unsigned timestamps from file
readStamps :: FilePath -> IO (V.Vector Time)
readStamps path = do
        contents <- BS.readFile path
        let readStamp :: [Word64] -> B.Get [Word64]
            readStamp xs = do
                    empty <- B.isEmpty 
                    if empty then return xs
                             else do x <- B.getWord64le
                                     readStamp (x:xs)

        let stampsToPackedVec :: [Word64] -> V.Vector Time
            stampsToPackedVec v = V.fromList $ map fromIntegral v
        return $ stampsToPackedVec $ reverse $ B.runGet (readStamp []) contents

spansChart spans = layout
                   where
                   coords :: [(Double, (Double,Double))]
                   coords = concat $ map f spans
                            where f (a,b) = let a' = realToFrac a
                                                b' = realToFrac b
                                            in [(a', (0,0)), (a', (0,1)), (b', (0,1)), (b', (0,0))]
                   fill = plot_fillbetween_values ^= coords
                        $ defaultPlotFillBetween
                   -- photonPoints = plot_points_values ^= map (1,) dts
                   layout = layout1_plots ^= [Left $ toPlot fill]
                          $ defaultLayout1

main :: IO ()
main = do let n = 15
          fname:_ <- getArgs
          process fname n

process :: FilePath -> Int -> IO ()
process fname n = do
          stamps <- readStamps fname
          printf "Read %u timestamps\n" (V.length stamps)
          let head_t = V.head stamps
              last_t = V.last stamps
              duration = (jiffy * (fromIntegral $ last_t-head_t))
          printf "Timestamp range %u..%u : %4.2e seconds\n" head_t last_t duration
          printf "Average rate %1.3f photons/second\n" $ (fromIntegral $ V.length stamps) / duration

          let dts = V.map (uncurry (-)) $ V.zip (V.tail stamps) stamps
          --dts <- (liftM V.fromList) testData2
          --let dts = V.fromList testData
          let accept t = beta n dts def_mp t > 2 -- TODO: Why is this so small?
              bursts = filter accept [0..V.length dts-n-1]

          let (bg_tau, burst_tau) = guessTaus dts
              bg_real_rate = 1 / (jiffy * fromIntegral bg_tau)
              burst_real_rate = 1 / (jiffy * fromIntegral burst_tau)
          printf "Background: %f\nBurst: %f\n" bg_real_rate burst_real_rate

          f <- openFile "points" WriteMode
          mapM_ (\t->hPrintf f "%9u\t%9u\t%1.5e\n" t (dts!t) (beta n dts def_mp t)) [1..10000]
          hClose f

          print def_mp
          --print $ V.take 1100 dts
          --print $ take 1500 bursts
          
          if length bursts == 0 then print "No bursts found"
                                else printf "Found %u burst photons\n" (length bursts)
          let cspans = compressFuzzySpans bursts 35
              cspans' = filter (\(a,b)->(b-a) > 15) cspans

          f <- openFile "spans" WriteMode
          mapM_ (uncurry $ hPrintf f "%9u\t%9u\n") cspans'
          hClose f

          --renderableToPNGFile (toRenderable $ spansChart (take 10 cspans)) 1600 1200 "spans.png"
          return ()

