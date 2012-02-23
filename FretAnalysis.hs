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

jiffy = 1/128e6 -- s
beta_thresh = 2
burstRate = 4000 -- 1/s
bgRate = 200 -- 1/s
fretChs = Fret { fretA = Ch0
               , fretD = Ch1
               }

mp = ModelParams { window = 10
                 , prob_b = 0.1
                 , tau_burst = round $ 1 / burstRate / jiffy
                 , tau_bg = round $ 1 / bgRate / jiffy
                 }
     
summary label photons =
  let len = realToFrac $ V.length photons :: Double
      dur = photonsDuration jiffy photons
  in printf "%s: %1.1e photons, %1.2e sec, %1.2e Hz\n" label len dur (len/dur)
     
fretBursts :: Fret (V.Vector Time) -> Fret [V.Vector Time]
fretBursts d =
  let combined = combineChannels [ fretD d
                                 , fretA d
                                 ]
      
      burstTimes = V.map (combined V.!)
                   $ findBurstPhotons mp beta_thresh
                   $ timesToInterarrivals combined
      spans = compressSpans (40*tau_burst mp) (V.toList burstTimes)
  in fmap (flip spansPhotons $ spans) d
     
main = do
  (fname:_) <- getArgs
  recs <- readRecords fname
  
  summary "Raw" $ V.map recTime recs
  let fret = fmap (strobeTimes recs) fretChs
          
  summary "A" $ fretA fret
  summary "D" $ fretD fret
  
  print mp
  let bursts = fretBursts fret
      burstStats bursts =
        let counts = V.fromList $ map (realToFrac . V.length) bursts
        in (mean counts, stdDev counts)
  print $ fmap burstStats bursts
  simpleHist "d.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ fretD bursts
  simpleHist "a.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ fretA bursts
  
  let separate = separateBursts bursts
  simpleHist "fret_eff.png" 20 $ map proxRatio separate
  return ()
  
separateBursts :: Fret [V.Vector Time] -> [Fret Double]
separateBursts x =
  let Fret a b = fmap (map (realToFrac . V.length)) x
  in zipWith Fret a b
 