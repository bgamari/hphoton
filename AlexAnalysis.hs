import Data.List
import System.Environment
import HPhoton.Types
import HPhoton.BayesBurstFind
import HPhoton.Utils
import HPhoton.Fret.Alex
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
alexChs = AlexChannels { alexExc = Fret { fretA = Ch0
                                        , fretD = Ch1
                                        }
                       , alexEm =  Fret { fretA = Ch0
                                        , fretD = Ch1
                                        }
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
     
alexBursts :: Alex (V.Vector Time) -> Alex [V.Vector Time]
alexBursts d =
  let combined = combineChannels [ alexAexcAem d
                                 , alexAexcDem d
                                 , alexDexcDem d
                                 , alexDexcAem d
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
  cachedAlex <- getCachedAlex fname
  let alex = maybe (alexTimes 0 alexChs recs) id cachedAlex
  putCachedAlex fname alex
          
  summary "AexcAem" $ alexAexcAem alex
  summary "AexcDem" $ alexAexcDem alex
  summary "DexcAem" $ alexDexcAem alex
  summary "DexcDem" $ alexDexcDem alex
  
  print mp
  let bursts = alexBursts alex
      burstStats bursts =
        let counts = V.fromList $ map (realToFrac . V.length) bursts
        in (mean counts, stdDev counts)
  print $ fmap burstStats bursts
  simpleHist "da.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ alexDexcAem bursts
  simpleHist "aa.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ alexAexcAem bursts
  simpleHist "ad.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ alexAexcDem bursts
  simpleHist "dd.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ alexDexcDem bursts
  
  let separate = separateBursts bursts
  simpleHist "fret_eff.png" 20 $ map proxRatio separate
  simpleHist "stoiciometry.png" 20 $ map stoiciometry separate
  return ()
  
separateBursts :: Alex [V.Vector Time] -> [Alex Double]
separateBursts alex =
  let Alex a b c d = fmap (map (realToFrac . V.length)) alex
  in zipWith4 Alex a b c d
 
