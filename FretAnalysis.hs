{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

import           Control.Monad (guard)
import           Data.Accessor
import           Data.Foldable
import           Data.List (genericLength)
import           Data.Maybe
import           Data.Traversable
import           Control.Applicative
import qualified Data.Vector.Unboxed as V
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Plot.Histogram
import           Graphics.Rendering.Chart.Simple.Histogram
import           HPhoton.Bin
import           HPhoton.BurstIdent.Bayes
import           HPhoton.BurstIdent.BinThreshold
import           HPhoton.FpgaTimetagger
import           HPhoton.Fret
import           HPhoton.Types
import           HPhoton.Utils
import           Prelude hiding (foldl1)
import           Statistics.Sample
import           System.Console.CmdArgs hiding (summary)
import           System.Environment
import           Text.Printf

-- | A rate measured in real time
type Rate = Double

data BurstMode = Bayes | BinThresh deriving (Show, Eq, Data, Typeable)
data FretAnalysis = FretAnalysis { clockrate :: Freq
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
                             
fretAnalysis = FretAnalysis { clockrate = round $ (128e6::Double) &= groupname "General" &= help "Timetagger clockrate (Hz)"
                            , n_bins = 20 &= groupname "General" &= help "Number of bins in efficiency histogram"
                            , input = def &= argPos 0 &= typFile
                            , burst_mode = enum [ BinThresh &= help "Use binning/thresholding for burst detection"
                                                , Bayes &= help "Use Bayesian burst detection"
                                                ]
                            
                            , bin_width = 10 &= groupname "Bin/threshold burst detection"
                                             &= help "Bin width in milliseconds"
                            , burst_thresh = 2 &= groupname "Bin/threshold burst detection"
                                               &= help "Threshold rate over background rate (multiples of sigma)"
                            
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
              , mpTauBurst = round $ 1 / burst_rate p / jiffy
              , mpTauBg = round $ 1 / bg_rate p / jiffy
              }
  where jiffy = realToFrac $ clockrate p
     
summary :: FretAnalysis -> String -> Clocked (V.Vector Time) -> IO ()
summary p label photons =
  let len = realToFrac $ V.length (unClocked photons) :: Double
      dur = realDuration $ fmap (:[]) photons
  in printf "%-8s: %1.1e photons, %1.2e sec, %1.2e Hz\n" label len dur (len/dur)
     
fretBursts :: FretAnalysis -> Clocked (Fret (V.Vector Time)) -> IO [Span]
fretBursts p@(FretAnalysis {burst_mode=Bayes}) d = do
  let mp = modelParamsFromParams p
      combined = combineChannels $ toList $ unClocked d
      burstTimes = V.backpermute combined
                   $ findBurstPhotons mp (beta_thresh p)
                   $ timesToInterarrivals combined
      spans = V.toList $ compressSpans (10*mpTauBurst mp) burstTimes
  putStrLn "Bayesian burst identification parameters:"
  print mp
  return spans

fretBursts p@(FretAnalysis {burst_mode=BinThresh}) d = do
  let combined = combineChannels $ toList $ unClocked d
      binWidthTicks = round $ 1e-3*bin_width p / jiffy d
      len = realToFrac $ V.length combined :: Double
      dur = photonsDuration (jiffy d) combined
      thresh = MultMeanThresh $ burst_thresh p
      spans = V.toList $ findBursts binWidthTicks thresh combined
  printf "Bin/threshold burst identification: bin width=%f ms, threshold=%s\n" (bin_width p) (show thresh)
  return spans
     
fretEffHist nbins e = layout
  where hist = plot_hist_values  ^= [e]
               $ plot_hist_range ^= Just (-0.1, 1.1)
               $ plot_hist_bins  ^= nbins
               $ defaultPlotHist
        layout = layout1_plots ^= [Left (plotHist hist)]
                 $ defaultLayout1
        
main' = do
  p <- cmdArgs fretAnalysis
  let mp = modelParamsFromParams p
  guard $ isJust $ input p
  recs <- readRecords $ fromJust $ input p
  let fret = Clocked (clockrate p)
             $ fmap (strobeTimes recs) fretChs
  let g = case () of 
            _ | Just f <- zero_fret p -> gammaFromFret 0 f
            _ | Just g <- gamma p     -> g
            otherwise                 -> 1

  printf "Gamma: %f\n" g
  analyzeData p g fret
  
backgroundRate :: Clocked (V.Vector Time) -> [Span] -> Double
backgroundRate times bursts =
  let range = ( V.head $ unClocked times, V.last $ unClocked times)
      background :: Clocked [V.Vector Time]
      background = fmap (flip spansPhotons $ invertSpans range bursts) times
      dur = realDuration background
      span_rates :: [Double]
      span_rates = map (\b->realToFrac (V.length b) / dur) $ unClocked background
  in mean $ V.fromList span_rates
  
crosstalkParam :: Clock (V.Vector Time) -> [Span] -> Double
crosstalkParam v dOnlySpans =
  map proximityRatio sep
  $ separateBursts
  $ fmap (fmap (flip spansPhotons $ dOnlySpans)) v

analyzeData :: FretAnalysis -> Gamma -> Clocked (Fret (V.Vector Time)) -> IO ()
analyzeData p g fret = do 
  summary p "A" $ fretA $ sequenceA fret
  summary p "D" $ fretD $ sequenceA fret

  let duration = realDuration $ fmap toList fret
  spans <- fretBursts p fret
  let bursts = fmap (fmap (flip spansPhotons $ spans)) fret
      bg_rate :: Fret Double
      bg_rate = fmap (flip backgroundRate $ spans) $ sequenceA fret
  let burstStats bursts =
        let counts = V.fromList $ map (realToFrac . V.length) bursts
        in (mean counts, stdDev counts)
  print $ fmap burstStats $ unClocked bursts
  print bg_rate

  simpleHist "d-bursts.png" 20
             $ filter (<100) $ map (realToFrac . V.length)
             $ fretD $ unClocked bursts
  simpleHist "a-bursts.png" 20
             $ filter (<100) $ map (realToFrac . V.length)
             $ fretA $ unClocked bursts
  
  let separate :: [Fret Double]
      separate = fmap (\a->(-) <$> a <*> bg_rate) 
                 $ separateBursts
                 $ fmap (filter (\burst->V.length burst > burst_size p))
                 $ unClocked bursts
  printf "Found %d bursts (%1.1f per second)\n"
    (length separate)
    (genericLength separate / duration)
  
  renderableToPNGFile (toRenderable
                       $ fretEffHist (n_bins p)
                       $ map (fretEfficiency g) separate
                      ) 640 480 "fret_eff.png"
  return ()
  
separateBursts :: Fret [V.Vector Time] -> [Fret Double]
separateBursts x =
  let Fret a b = fmap (map (realToFrac . V.length)) x
  in zipWith Fret a b
 
testData :: Clocked (Fret (V.Vector Time))
testData = Clocked 100000 $ Fret { fretA = V.generate 100000 (\i->fromIntegral $ i*1000)
                                 , fretD = V.generate 100000 (\i->fromIntegral $ i*1000)
                                 }

testData' :: Clocked (Fret (V.Vector Time))
testData' = Clocked freq $ Fret { fretA=times e, fretD=times (1-e) }
          where freq = 1000000
                e = 0.3
                times :: Double -> V.Vector Time
                times a = V.scanl1 (+)
                          $ V.concat $ replicate 10
                          $ V.replicate (round $ 1000 * a) (round $ realToFrac freq * 1e-3 / a) V.++ V.singleton freq

testMain = do
  let p = fretAnalysis { burst_thresh = 0.5
                       , burst_size = 0
                       }
  analyzeData p 1 testData'
  
main = main'
