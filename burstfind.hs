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

import Graphics.Rendering.Chart
import "data-accessor" Data.Accessor
import Debug.Trace

type Time = Int
type RealTime = Double
type Prob = Double

--jiffy = 3.33e-8 :: RealTime
jiffy = 10000 :: RealTime

testTauBG = round $ jiffy/20
modelTauBG = round $ jiffy/20
testTauBurst = round $ jiffy/60
modelTauBurst = round $ jiffy/60

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

-- | Reduce a list of times to a list of (startTime, endTime) spans
compressSpans :: [Time] -> [(Time, Time)]
compressSpans ts = let f :: (Time, Time, [(Time,Time)]) -> Time -> (Time, Time, [(Time,Time)])
                       f (startT, lastT, conseqs) t
                                | t == lastT = error $ "compressSpans: Uh oh! dt=0 at " ++ show t
                                | t == lastT + 1 = (startT, t, conseqs)
                                | otherwise = (t, t, (startT, lastT):conseqs)
                       (_,_,compressed) = foldl' f (-1, -1, []) ts
                   in tail $ reverse compressed

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
main = do --fname:_ <- getArgs
          --stamps <- readStamps fname
          let n = 15

          td <- testData2
          --let td = testData
          let dts = V.fromList td
          let accept t = beta n dts def_mp t > 2 -- TODO: Why is this so small?
              bursts = filter accept [0..V.length dts-n-1]

          f <- openFile "points" WriteMode
          let printT t = hPrintf f "%9u\t%9u\t%1.5e\n" t (dts!t) (beta n dts def_mp t)
          mapM_ printT [1..10000]
          hClose f

          print def_mp
          --print $ V.take 1100 dts
          --print $ take 1500 bursts
          
          let cspans = compressSpans bursts
          f <- openFile "spans" WriteMode
          mapM_ (uncurry $ hPrintf f "%9u\t%9u\n") cspans
          hClose f

          renderableToPNGFile (toRenderable $ spansChart (take 10 cspans)) 1600 1200 "spans.png"
          return ()

