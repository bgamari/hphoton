{-# LANGUAGE PackageImports, TupleSections #-}

import Control.Monad (replicateM, liftM, join, (<=<))
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import HPhoton.BayesBurstFind
import HPhoton.Types
import HPhoton.Bin

import Graphics.Rendering.Chart
import "data-accessor" Data.Accessor
import Data.Colour.Names
import Data.Colour

import System.Environment
import System.IO
import Text.Printf
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

import qualified System.Random.MWC as MWC
import Data.Random
import Data.Random.Distribution.Exponential
import HPhoton.FpgaTimetagger
import Data.Vector.Algorithms.Merge (sort)

type RealTime = Double

n = 20
betaThresh = 2
maxTime = 2 -- Seconds of data to plot

-- For real data
jiffy = 1/128e6 :: RealTime  -- Clock period
modelTauBG = realRateToTau 700
modelTauBurst = realRateToTau 6000

-- For testing
--jiffy = 1e-3 :: RealTime
--modelTauBG = realRateToTau 20
--modelTauBurst = realRateToTau 80

testTauBG = realRateToTau 20
testTauBurst = realRateToTau 100


realRateToTau rate = round $ 1/(rate*jiffy)
tauToRealRate tau = 1 / (tau / jiffy)

-- | Default model parameters
def_mp = ModelParams { prob_b = 0.05
                     , tau_bg = modelTauBG
                     , tau_burst = modelTauBurst
                     }

data CompressSpansState = CSpansState { startT :: Time
                                      , lastT  :: Time
                                      , result :: [(Time,Time)]
                                      } deriving Show

-- | Reduce a list of times to a list of (startTime, endTime) spans
compressSpans :: Time -> [Time] -> [(Time, Time)]
compressSpans fuzz ts =
        let f :: CompressSpansState -> Time -> CompressSpansState
            f s t  | t - lastT s <= fuzz  = s {lastT=t}
                   | otherwise = s {startT=t, lastT=t, result=(startT s,lastT s):result s}
            s = CSpansState {startT= -1, lastT= -1, result=[]}
            CSpansState _ _ compressed = foldl' f s ts
        in if null compressed then []
                              else tail $ reverse compressed 

mean, stdev :: (RealFrac a, V.Unbox a) => V.Vector a -> a
mean v | V.null v = error "Can't take mean of zero length array"
mean v = V.sum v / (realToFrac $ V.length v)
stdev v = let m = mean v
              ss = V.sum $ V.map (\x->(m-x)^2) v
          in ss / (realToFrac $ V.length v)

guessTaus :: V.Vector Time -> IO (Time, Time)
guessTaus dts = let width = round (1e-3/jiffy) :: Time
                    bins = binTimes (V.scanl1' (+) dts) width
                    rbins = V.map realToFrac bins :: V.Vector Double
                    thresh = mean rbins + 1*stdev rbins
                    bg_tau = realToFrac width / (mean $ V.filter (<thresh) rbins)
                    burst_tau = realToFrac width / (mean $ V.filter (>thresh) rbins)
                in do 
                    printf "Threshold: %f\n" thresh
                    printf "Background: %f\nBurst: %f\n" (1 / jiffy / bg_tau) (1 / jiffy / burst_tau)
                    return (round bg_tau, round burst_tau)


-- | A perfectly periodic set of inter-arrival times
testData :: [Time]
testData = let v = (replicate 900 testTauBG) ++ (replicate 100 testTauBurst)
           in take 100000 $ cycle v

-- | Produce realistic inter-arrival times
testData2 :: IO [Time]
testData2 = do mwc <- MWC.create
               let -- | Inter-arrival time
                   iaa :: Time -> RVar Double
                   iaa = exponential . realToFrac
                   sCycle :: RVar [Double]
                   sCycle = liftM join $ sequence [ replicateM 900 (iaa testTauBG)
                                                  , replicateM 30 (iaa testTauBurst) ]
               a <- sampleFrom mwc $ replicateM 100 sCycle
               return $ map ceiling $ join a


spansChartRealTime :: [(Time,Time)] -> V.Vector (Time, Double) -> RealTime -> Layout1 Double Double
spansChartRealTime spans betas maxTime = spansChart spans' betas'
        where betas' = V.takeWhile (\(x,y) -> x<maxTime)
                     $ V.map (\(x,y)->(jiffy*realToFrac x, y)) betas
              spans' = takeWhile (\(x,y) -> y<maxTime)
                     $ map (\(x,y)->(jiffy*realToFrac x, jiffy*realToFrac y)) spans

spansChart :: [(RealTime,RealTime)] -> V.Vector (RealTime, Double) -> Layout1 Double Double
spansChart spans betas = layout
        where
        -- For model plot
        coords :: [(Double, (Double,Double))]
        coords = concat $ map f spans
                 where f (a,b) = let a' = realToFrac a
                                     b' = realToFrac b
                                 in [ (a', (-100,-100)), (a', (-100,100))
                                    , (b', (-100,100)), (b', (-100,-100)) ]
        fill = plot_fillbetween_values ^= coords
             $ plot_fillbetween_title  ^= "Detected bursts"
             $ defaultPlotFillBetween
        betaPlot = plot_points_style  ^= filledCircles 1 (opaque red)
                 $ plot_points_values ^= V.toList ( V.filter (\(x,y)->y > -100)
                                                  $ V.map (\(x,y)->(realToFrac x, log y))
                                                  $ betas )
                 $ plot_points_title ^= "log Beta"
                 $ defaultPlotPoints
        threshold = hlinePlot "Beta threshold" (defaultPlotLineStyle { line_color_=opaque green }) betaThresh
        -- photonPoints = plot_points_values ^= map (1,) dts
        layout = layout1_plots ^= [ Left $ toPlot fill 
                                  , Left $ toPlot betaPlot 
                                  , Left $ threshold ]
               $ defaultLayout1

binsChart :: V.Vector Time -> RealTime -> Layout1 Double Double
binsChart times maxTime = layout
        where
        binSize = round $ 5e-3 / jiffy
        times' = V.takeWhile (\t->t < round (maxTime / jiffy)) times
        bins = binTimesWithTimes times' binSize
        rbins = V.map (realToFrac . snd) bins :: V.Vector Double
        thresh = 1.5 * mean rbins
        lines cond = let f ((x1,y),(x2,_)) | cond y = Just [ (jiffy*realToFrac x1, realToFrac y)
                                                           , (jiffy*realToFrac x2, realToFrac y) ]
                         f _ = Nothing
                     in mapMaybe f $ V.toList $ V.zip bins (V.tail bins)
        burstCurve = plot_lines_style  ^= solidLine 2 (opaque red)
                   $ plot_lines_values ^= lines ((>thresh) . realToFrac)
                   $ plot_lines_title  ^= "Burst bins"
                   $ defaultPlotLines
        bgCurve = plot_lines_style ^= solidLine 2 (opaque green)
                   $ plot_lines_values ^= lines ((<thresh) . realToFrac)
                   $ plot_lines_title  ^= "Background bins"
                   $ defaultPlotLines
        threshold = hlinePlot "Threshold" (defaultPlotLineStyle { line_color_=opaque green }) thresh
        layout = layout1_plots ^= [ Left $ toPlot burstCurve
                                  , Left $ toPlot bgCurve
                                  , Left $ threshold ]
               $ defaultLayout1

mainFile, mainTest :: IO ()
mainFile = do fname:_ <- getArgs
              rs <- readRecords fname
              let stampsA = strobeChTimes rs Ch0
                  stampsD = strobeChTimes rs Ch1
              stamps <- combineChannels [stampsA, stampsD]
              process stamps n

-- | Combine multiple timestamp channels
combineChannels :: [V.Vector Time] -> IO (V.Vector Time)
combineChannels chs = do stamps <- V.thaw $ V.concat chs
                         sort stamps
                         stamps' <- V.freeze stamps
                         return stamps'
                         --return $ V.map (- V.head stamps) stamps

mainTest = do 
          stamps <- (liftM $ V.scanl' (+) 0 . V.fromList) testData2
          --let stamps = V.fromList testData
          process stamps n

main = mainFile

process :: V.Vector Time -> Int -> IO ()
process times n = do
          let dts = V.zipWith (-) (V.tail times) times
              head_t = 0
              last_t = V.foldl1' (+) dts
              duration = (jiffy * (fromIntegral $ last_t-head_t))
          printf "%d photons" (V.length times)
          printf "Timestamp range %u..%u : %4.2e seconds\n" head_t last_t duration
          printf "Average rate %1.3f photons/second\n" $ (fromIntegral $ V.length dts) / duration
          print def_mp

          let betas = V.map (\i->(times ! fromIntegral i, beta n dts def_mp i))
                    $ V.generate (V.length dts-n) fromIntegral
              bursts = V.filter (\(t,beta) -> beta>betaThresh) betas
              burstTimes = V.map fst bursts 

          --(bg_tau, burst_tau) <- guessTaus dts

          --f <- openFile "points" WriteMode
          --mapM_ (\i->hPrintf f "%9u\t%9u\t%1.5e\n" i (dts!i) (beta n dts def_mp (fromIntegral i))) [1..10000]
          --hClose f
          
          if V.length bursts == 0
             then putStrLn "No bursts found"
             else do printf "Found %u burst photons\n" (V.length bursts)
                     let cspans = compressSpans (40*modelTauBurst) (V.toList burstTimes)
                         cspans' = filter (\(a,b)->(b-a) > 15) cspans
                     printf "Average %f photons/burst\n" (realToFrac (V.length bursts) / realToFrac (length cspans) :: Double)

                     f <- openFile "spans" WriteMode
                     mapM_ (uncurry $ hPrintf f "%9u\t%9u\n") cspans
                     hClose f

                     let r = renderLayout1sStacked [ withAnyOrdinate $ spansChartRealTime cspans betas maxTime
                                                   , withAnyOrdinate $ binsChart times maxTime ]
                     renderableToPNGFile r 1600 1200 "spans.png"
                     return ()


