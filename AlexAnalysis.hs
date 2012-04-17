{-# LANGUAGE DeriveDataTypeable #-}

import Data.Maybe
import Data.List
import Data.Traversable (sequenceA)
import System.Environment
import HPhoton.Types
import HPhoton.BurstIdent.Bayes
import HPhoton.Utils
import HPhoton.Fret.Alex
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

data FretAnalysis = FretAnalysis { clockrate :: Freq
                                 , beta_thresh :: Double
                                 , bg_rate :: Rate
                                 , burst_rate :: Rate
                                 , prob_b :: Double
                                 , window :: Int
                                 , input :: Maybe FilePath
                                 }
                    deriving (Show, Eq, Data, Typeable)
                             
fretAnalysis = FretAnalysis { clockrate = round (128e6::Double) &= help "Jiffy time (s)"
                            , beta_thresh = 2 &= help "Beta threshold"
                            , burst_rate = 4000 &= help "Burst rate (1/s)"
                            , bg_rate = 200 &= help "Background rate (1/s)"
                            , window = 10 &= help "Burst window (photons)"
                            , prob_b = 0.01 &= help "Probability of burst"
                            , input = def &= argPos 0 &= typFile
                            }
               
alexChs = AlexChannels { alexExc = Fret { fretA = Ch1
                                        , fretD = Ch0
                                        }
                       , alexEm =  Fret { fretA = Ch1
                                        , fretD = Ch0
                                        }
                       }

modelParamsFromParams :: FretAnalysis -> ModelParams
modelParamsFromParams p =
  ModelParams { mpWindow = window p
              , mpProbB = prob_b p
              , mpTauBurst = round $ 1 / burst_rate p / jiffy
              , mpTauBg = round $ 1 / bg_rate p / jiffy
              }
  where jiffy = 1 / (realToFrac $ clockrate p)
     
summary :: FretAnalysis -> String -> Clocked (V.Vector Time) -> IO ()
summary p label photons =
  let len = realToFrac $ V.length $ unClocked photons :: Double
      dur = realDuration $ fmap (:[]) photons
  in printf "%s: %1.1e photons, %1.2e sec, %1.2e Hz\n" label len dur (len/dur)
     
alexBursts :: FretAnalysis -> Clocked (Alex (V.Vector Time)) -> Alex [V.Vector Time]
alexBursts p (Clocked _ d) =
  let mp = modelParamsFromParams p
      combined = combineChannels [ alexAexcAem d
                                 , alexAexcDem d
                                 , alexDexcDem d
                                 , alexDexcAem d
                                 ]
      burstTimes = V.map (combined V.!)
                   $ findBurstPhotons mp (beta_thresh p)
                   $ timesToInterarrivals combined
      spans = V.toList $ compressSpans (40*mpTauBurst mp) burstTimes
  in fmap (flip spansPhotons $ spans) d

main = do
  p <- cmdArgs fretAnalysis
  let mp = modelParamsFromParams p
  guard $ isJust $ input p
  recs <- readRecords $ fromJust $ input p
  let raw = Clocked (clockrate p) $ V.map recTime recs
  
  cachedAlex <- getCachedAlex $ fromJust $ input p
  let alex = Clocked (clockrate p)
             $ maybe (alexTimes 0 alexChs recs) id cachedAlex
  putCachedAlex (fromJust $ input p) $ unClocked alex
          
  summary p "Raw" raw
  summary p "AexcAem" $ alexAexcAem $ sequenceA alex
  summary p "AexcDem" $ alexAexcDem $ sequenceA alex
  summary p "DexcAem" $ alexDexcAem $ sequenceA alex
  summary p "DexcDem" $ alexDexcDem $ sequenceA alex
  
  print mp
  let bursts = alexBursts p alex
      burstStats bursts =
        let counts = V.fromList $ map (realToFrac . V.length) bursts
        in (mean counts, stdDev counts)
  print $ fmap burstStats bursts
  simpleHist "da.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ alexDexcAem bursts
  simpleHist "aa.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ alexAexcAem bursts
  simpleHist "ad.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ alexAexcDem bursts
  simpleHist "dd.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ alexDexcDem bursts
  
  let separate = separateBursts bursts
  printf "Found %d bursts" (length separate)
  simpleHist "fret_eff.png" 80 $ map proxRatio separate
  simpleHist "stoiciometry.png" 80 $ map stoiciometry separate
  return ()
  
separateBursts :: Alex [V.Vector Time] -> [Alex Double]
separateBursts alex =
  let Alex a b c d = fmap (map (realToFrac . V.length)) alex
  in zipWith4 Alex a b c d
 
