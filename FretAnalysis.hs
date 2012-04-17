{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

import Debug.Trace
import HPhoton.Bin

import Data.Maybe
import Data.List
import System.Environment
import HPhoton.Types
import HPhoton.BurstIdent.Bayes
import HPhoton.BurstIdent.BinThreshold
import HPhoton.Utils
import HPhoton.Fret
import Text.Printf
import HPhoton.FpgaTimetagger
import HPhoton.FpgaTimetagger.Alex
import Statistics.Sample
import qualified Data.Vector.Unboxed as V
import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Plot.Histogram
import Graphics.Rendering.Chart.Simple.Histogram
import System.Console.CmdArgs hiding (summary)
import Control.Monad (guard)

-- | A rate measured in real time
type Rate = Double

data BurstMode = Bayes | BinThresh deriving (Show, Eq, Data, Typeable)
data FretAnalysis = FretAnalysis { jiffy :: RealTime
                                 , n_bins :: Int
                                 , input :: Maybe FilePath
                                 , burst_mode :: BurstMode

                                 , bin_width :: RealTime
                                 , burst_thresh :: Double
                                 
                                 , beta_thresh :: Double
                                 , bg_rate :: Rate
                                 , burst_size :: Int
                                 , burst_rate :: Rate
                                 , prob_b :: Double
                                 , window :: Int

                                 , zero_fret :: Maybe ProxRatio
                                 , gamma :: Maybe Gamma
                                 }
                    deriving (Show, Eq, Data, Typeable)
                             
fretAnalysis = FretAnalysis { jiffy = 1/128e6 &= groupname "General" &= help "Jiffy time (s)"
                            , n_bins = 40 &= groupname "General" &= help "Number of bins in efficiency histogram"
                            , input = def &= argPos 0 &= typFile
                            , burst_mode = enum [ Bayes &= help "Use Bayesian burst detection"
                                                , BinThresh &= help "Use binning/thresholding for burst detection"
                                                ]
                            
                            , bin_width = 10 &= groupname "Bin/threshold burst detection"
                                             &= help "Bin width in milliseconds"
                            , burst_thresh = 2 &= groupname "Bin/threshold burst detection"
                                               &= help "Threshold rate over background rate"
                            
                            , burst_size = 10 &= groupname "Bayesian burst detection"
                                              &= help "Minimum burst size"
                            , burst_rate = 4000 &= groupname "Bayesian burst detection"
                                                &= help "Burst rate (1/s)"
                            , bg_rate = 200 &= groupname "Bayesian burst detection"
                                            &= help "Background rate (1/s)"
                            , window = 10 &= groupname "Bayesian burst detection"
                                          &= help "Burst window (photons)"
                            , prob_b = 0.01 &= groupname "Bayesian burst detection"
                                            &= help "Probability of burst"
                            , beta_thresh = 2 &= groupname "Bayesian burst detection"
                                              &= help "Beta threshold"

                            , zero_fret = Nothing &= groupname "Gamma correction"
                                                  &= help "Measured efficiency for zero FRET (donor-only)"
                            , gamma = Nothing &= groupname "Gamma correction"
                                              &= help "Gamma"
                            }
               
fretChs = Fret { fretA = Ch1
               , fretD = Ch0
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
  in printf "%-8s: %1.1e photons, %1.2e sec, %1.2e Hz\n" label len dur (len/dur)
     
fretBursts :: FretAnalysis -> Fret (V.Vector Time) -> IO (Fret [V.Vector Time])
fretBursts p@(FretAnalysis {burst_mode=Bayes}) d = do
  let mp = modelParamsFromParams p
      combined = combineChannels [fretD d, fretA d]
      burstTimes = V.map (combined V.!)
                   $ findBurstPhotons mp (beta_thresh p)
                   $ timesToInterarrivals combined
      spans = V.toList $ compressSpans (10*mpTauBurst mp) burstTimes
  putStrLn "Bayesian burst identification parameters:"
  print mp
  return $ fmap (flip spansPhotons $ spans) d

fretBursts p@(FretAnalysis {burst_mode=BinThresh}) d = do
  let combined = combineChannels [fretD d, fretA d]
      binWidthTicks = round $ 1e-3*bin_width p / jiffy p
      len = realToFrac $ V.length combined :: Double
      dur = photonsDuration (jiffy p) combined
      thresh = round $ burst_thresh p * len / dur * bin_width p * 1e-3
      spans = V.toList $ findBursts binWidthTicks thresh combined
  printf "Bin/threshold burst identification: bin width=%f ms, threshold=%d/bin\n" (bin_width p) thresh
  return $ fmap (flip spansPhotons $ spans) d
     
fretEffHist nbins e = layout
  where hist = plot_hist_values  ^= [e]
               $ plot_hist_range ^= Just (-0.1, 1.1)
               $ plot_hist_bins  ^= nbins
               $ defaultPlotHist
        layout = layout1_plots ^= [Left (plotHist hist)]
                 $ defaultLayout1
        
main = do
  p <- cmdArgs fretAnalysis
  let mp = modelParamsFromParams p
  guard $ isJust $ input p
  recs <- readRecords $ fromJust $ input p
  
  let g = case () of 
            _ | Just f <- zero_fret p -> gammaFromFret 0 f
            _ | Just g <- gamma p     -> g
            otherwise                 -> 1
  
  printf "Gamma: %f\n" g
  summary p "Raw" $ V.map recTime recs
  let fret = fmap (strobeTimes recs) fretChs
  summary p "A" $ fretA fret
  summary p "D" $ fretD fret
  
  bursts <- fretBursts p fret
  let burstStats bursts =
        let counts = V.fromList $ map (realToFrac . V.length) bursts
        in (mean counts, stdDev counts)
  print $ fmap burstStats bursts
  simpleHist "d.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ fretD bursts
  simpleHist "a.png" 20 $ filter (<100) $ map (realToFrac . V.length) $ fretA bursts
  
  let separate = separateBursts
                 $ fmap (filter (\burst->V.length burst > burst_size p))
                 $ bursts
  printf "Found %d bursts (%1.1f per second)\n"
    (length separate)
    (genericLength separate / photonsDuration (jiffy p) (V.map recTime recs))
  
  renderableToPNGFile (toRenderable
                       $ fretEffHist (n_bins p)
                       $ map (fretEfficiency g) separate
                      ) 640 480 "fret_eff.png"
  return ()
  
separateBursts :: Fret [V.Vector Time] -> [Fret Double]
separateBursts x =
  let Fret a b = fmap (map (realToFrac . V.length)) x
  in zipWith Fret a b
 