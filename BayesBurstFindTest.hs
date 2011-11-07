{-# LANGUAGE PackageImports, TupleSections #-}

import Control.Monad (replicateM, liftM, join, (<=<))
import Data.List (foldl')
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
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

import qualified System.Random.MWC as MWC
import Data.Random
import Data.Random.Distribution.Exponential
import HPhoton.FpgaTimetagger
import Data.Vector.Algorithms.Merge (sort)

type RealTime = Double

n = 15
betaThresh = 2

-- For real data
--jiffy = 1/128e6 :: RealTime  -- | 1/clockrate
--modelTauBG = realRateToTau 700
--modelTauBurst = realRateToTau 10000

-- For testing
jiffy = 1e-3 :: RealTime
modelTauBG = realRateToTau 20
modelTauBurst = realRateToTau 80
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
            f s t  | t == lastT s         = error $ "compressSpans: Repeated photons at " ++ show t
                   | t - lastT s <= fuzz  = s {lastT=t}
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


spansChart spans betas = layout
                   where
                   coords :: [(Double, (Double,Double))]
                   coords = concat $ map f spans
                            where f (a,b) = let a' = realToFrac a
                                                b' = realToFrac b
                                            in [ (a', (-100,-100)), (a', (-100,100))
                                               , (b', (-100,100)), (b', (-100,-100)) ]
                   fill = plot_fillbetween_values ^= coords
                        $ plot_fillbetween_title ^= "Detected bursts"
                        $ defaultPlotFillBetween
                   betaPlot = plot_points_style ^= filledCircles 1 (opaque red)
                            $ plot_points_values ^= V.toList (V.filter (\(x,y)->y > -100) $ V.map (\(x,y)->(realToFrac x,log y)) betas)
                            $ plot_points_title ^= "log Beta"
                            $ defaultPlotPoints
                   threshold = hlinePlot "Beta threshold" (defaultPlotLineStyle { line_color_=opaque green }) betaThresh
                   -- photonPoints = plot_points_values ^= map (1,) dts
                   layout = layout1_plots ^= [ Left $ toPlot fill 
                                             , Left $ toPlot betaPlot 
                                             , Left $ threshold ]
                          $ defaultLayout1

mainFile, mainTest :: IO ()
mainFile = do fname:_ <- getArgs
              rs <- readRecords fname
              let stampsA = strobeChTimes rs Ch1
                  stampsD = strobeChTimes rs Ch2
              stamps <- V.thaw $ V.concat [stampsA, stampsD]
              sort stamps
              stamps' <- V.freeze stamps
              let dts = V.map (uncurry (-)) $ V.zip (V.tail stamps') stamps'
              process dts n

mainTest = do 
          dts <- (liftM $ V.scanl' (+) 0 . V.fromList) testData2
          --let dts = V.fromList testData
          process dts n

main = mainFile

process :: V.Vector Time -> Int -> IO ()
process times n = do
          let dts = V.zipWith (-) (V.tail times) times
              head_t = 0
              last_t = V.foldl1' (+) dts
              duration = (jiffy * (fromIntegral $ last_t-head_t))
          printf "Timestamp range %u..%u : %4.2e seconds\n" head_t last_t duration
          printf "Average rate %1.3f photons/second\n" $ (fromIntegral $ V.length dts) / duration

          let betas = V.map (\t->(t, beta n dts def_mp t)) $ V.generate (V.length dts-n) fromIntegral
              bursts = V.map fst $ V.filter (\(x,y) -> y>betaThresh) betas

          (bg_tau, burst_tau) <- guessTaus dts

          f <- openFile "points" WriteMode
          mapM_ (\i->hPrintf f "%9u\t%9u\t%1.5e\n" i (dts!i) (beta n dts def_mp (fromIntegral i))) [1..10000]
          hClose f

          print def_mp
          --print $ V.take 1100 dts
          
          if V.length bursts == 0
             then putStrLn "No bursts found"
             else do printf "Found %u burst photons\n" (V.length bursts)
                     let cspans = compressSpans 35 (V.toList bursts)
                         cspans' = filter (\(a,b)->(b-a) > 15) cspans

                     f <- openFile "spans" WriteMode
                     mapM_ (uncurry $ hPrintf f "%9u\t%9u\n") cspans
                     hClose f

                     renderableToPNGFile (toRenderable $ spansChart (take 10 cspans) (V.take 10000 betas)) 1600 1200 "spans.png"
                     return ()


