{-# LANGUAGE DeriveDataTypeable #-}

import Data.Maybe
import Data.List
import System.Environment
import HPhoton.Types
import HPhoton.BayesBurstFind
import HPhoton.Utils
import HPhoton.Fret
import Text.Printf
import HPhoton.FpgaTimetagger
import HPhoton.FpgaTimetagger.Alex
import Statistics.Sample
import qualified Data.Vector.Unboxed as V
import Graphics.Rendering.Chart.Simple.Histogram
import System.Console.CmdArgs hiding (summary)
import Control.Monad (guard)

-- | A rate measured in real time
type Rate = Double

data FretAnalysis = FretAnalysis { jiffy :: RealTime
                                 , beta_thresh :: Double
                                 , bg_rate :: Rate
                                 , burst_rate :: Rate
                                 , prob_b :: Double
                                 , window :: Int
                                 , fname :: Maybe FilePath
                                 }
                    deriving (Show, Eq, Data, Typeable)
                             
fretAnalysis = FretAnalysis { jiffy = 1/128e6 &= help "Jiffy time (s)"
                            , beta_thresh = 2 &= help "Beta threshold"
                            , burst_rate = 4000 &= help "Burst rate (1/s)"
                            , bg_rate = 200 &= help "Background rate (1/s)"
                            , window = 10 &= help "Burst window (photons)"
                            , prob_b = 0.01 &= help "Probability of burst"
                            , fname = def &= argPos 0 &= typFile
                            }
               
fretChs = Fret { fretA = Ch0
               , fretD = Ch1
               }

modelParamsFromParams :: FretAnalysis -> ModelParams
modelParamsFromParams p =
  ModelParams { mpWindow = window p
              , mpProbB = prob_b p
              , mpTauBurst = round $ 1 / burst_rate p / jiffy p
              , mpTauBg = round $ 1 / bg_rate p / jiffy p
              }
     
summary p label photons =
  let len = realToFrac $ V.length photons :: Double
      dur = photonsDuration (jiffy p) photons
  in printf "%s: %1.1e photons, %1.2e sec, %1.2e Hz\n" label len dur (len/dur)
     
fretBursts :: FretAnalysis -> Fret (V.Vector Time) -> Fret [V.Vector Time]
fretBursts p d =
  let mp = modelParamsFromParams p
      combined = combineChannels [fretD d, fretA d]
      burstTimes = V.map (combined V.!)
                   $ findBurstPhotons mp (beta_thresh p)
                   $ timesToInterarrivals combined
      spans = V.toList $ compressSpans (40*mpTauBurst mp) burstTimes
  in fmap (flip spansPhotons $ spans) d
     
main = do
  p <- cmdArgs fretAnalysis
  let mp = modelParamsFromParams p
  guard $ isJust $ fname p
  recs <- readRecords $ fromJust $ fname p
  
  summary p "Raw" $ V.map recTime recs
  let fret = fmap (strobeTimes recs) fretChs
  summary p "A" $ fretA fret
  summary p "D" $ fretD fret
  
  print mp
  let bursts = fretBursts p fret
      burstStats bursts =
        let counts = V.fromList $ map (realToFrac . V.length) bursts
        in (mean counts, stdDev counts)
  print $ fmap burstStats bursts
  simpleHist "d.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ fretD bursts
  simpleHist "a.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ fretA bursts
  
  let separate = separateBursts bursts
  printf "Found %d bursts" (length separate)
  simpleHist "fret_eff.png" 20 $ map proxRatio separate
  return ()
  
separateBursts :: Fret [V.Vector Time] -> [Fret Double]
separateBursts x =
  let Fret a b = fmap (map (realToFrac . V.length)) x
  in zipWith Fret a b
 