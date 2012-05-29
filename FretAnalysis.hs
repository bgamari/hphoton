{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
import           Control.Applicative
import           Control.Monad (guard)
import           Data.Accessor
import           Data.Foldable
import           Data.List (genericLength, stripPrefix)
import           Data.Maybe
import           Data.Traversable
import qualified Data.Vector.Unboxed as V
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Plot.Histogram
import           Graphics.Rendering.Chart.Simple.Histogram
import           HPhoton.Bin
import           HPhoton.Bin.Plot
import           HPhoton.BurstIdent.Bayes
import           HPhoton.BurstIdent.BinThreshold
import           HPhoton.FpgaTimetagger
import           HPhoton.Fret
import           HPhoton.Types
import           HPhoton.Utils
import           Prelude hiding (foldl1, concat, all)
import           Statistics.Sample
import           System.Console.CmdArgs hiding (summary)
import           System.Environment
import           Text.Printf

-- | A rate measured in real time
type Rate = Double

data BurstMode = Bayes
               | BayesCombined
               | BinThresh deriving (Show, Eq, Data, Typeable)

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
                                                , Bayes &= help "Use Bayesian burst detection (acceptor channel)"
                                                , BayesCombined &= help "Use Bayesian burst detection (both channels)"
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
               
stripSuffix :: String -> String -> String             
stripSuffix suffix = reverse . maybe (error "Invalid filename") id . stripPrefix (reverse suffix) . reverse

rootName :: FretAnalysis -> String
rootName = stripSuffix ".timetag" . maybe (error "Need filename") id . input

fretChs = Fret { fretA = Ch1
               , fretD = Ch0
               }
     
summary :: Clock -> FretAnalysis -> String -> V.Vector Time -> IO ()
summary clk p label photons =
  let len = realToFrac $ V.length photons :: Double
      dur = realDuration clk [photons]
  in printf "%-8s: %1.1e photons, %1.2e sec, %1.2e Hz\n" label len dur (len/dur)
     
burstSpans :: ModelParams -> Double -> V.Vector Time -> [Span]
burstSpans mp betaThresh times =
  let burstTimes = V.backpermute times
                   $ findBurstPhotons mp betaThresh
                   $ timesToInterarrivals times
  in V.toList $ compressSpans (10*mpTauBurst mp) burstTimes

dOnlyBursts :: Clock -> FretAnalysis -> Fret (V.Vector Time) -> [Span]
dOnlyBursts clk p d =
  let mp = ModelParams { mpWindow = window p
                       , mpProbB = prob_b p
                       , mpTauBurst = round $ 1 / 140 * jiffy clk
                       , mpTauBg = round $ 1 / 70 * jiffy clk
                       }
  in burstSpans mp (beta_thresh p) (fretA d)

modelParams :: Clock -> FretAnalysis -> ModelParams
modelParams clk p =
  ModelParams { mpWindow = window p
              , mpProbB = prob_b p
              , mpTauBurst = round $ 1 / burst_rate p / jiffy clk
              , mpTauBg = round $ 1 / bg_rate p / jiffy clk
              }

fretBursts :: Clock -> FretAnalysis -> Fret (V.Vector Time) -> IO [Span]
fretBursts clk p@(FretAnalysis {burst_mode=BayesCombined}) d = do
  let mp = modelParams clk p
  putStrLn "Bayesian burst identification (combined) parameters:"
  print mp
  return $ burstSpans mp (beta_thresh p) (combineChannels $ toList d)

fretBursts clk p@(FretAnalysis {burst_mode=Bayes}) d = do
  let mp = modelParams clk p
  putStrLn "Bayesian burst identification (acceptor) parameters:"
  print mp
  return $ burstSpans mp (beta_thresh p) (fretA d)

fretBursts clk p@(FretAnalysis {burst_mode=BinThresh}) d = do
  let combined = combineChannels $ toList d
      binWidthTicks = round $ 1e-3*bin_width p / jiffy clk
      len = realToFrac $ V.length combined :: Double
      dur = realDuration clk [combined]
      thresh = MultMeanThresh $ burst_thresh p
      spans = V.toList $ findBursts binWidthTicks thresh combined
  printf "Bin/threshold burst identification: bin width=%f ms, threshold=%s\n" (bin_width p) (show thresh)
  return spans
     
fretEffHist :: Int -> [FretEff] -> Layout1 FretEff Int
fretEffHist nbins e = layout
  where hist = plot_hist_values  ^= [e]
               $ plot_hist_range ^= Just (-0.1, 1.1)
               $ plot_hist_bins  ^= nbins
               $ defaultPlotHist
        layout = layout1_plots ^= [Left (plotHist hist)]
                 $ defaultLayout1
        
main' = do
  p <- cmdArgs fretAnalysis
  guard $ isJust $ input p
  recs <- readRecords $ fromJust $ input p
  let fret = fmap (strobeTimes recs) fretChs
  let g = case () of 
            _ | Just f <- zero_fret p -> gammaFromFret 0 f
            _ | Just g <- gamma p     -> g
            otherwise                 -> 1

  printf "Gamma: %f\n" g
  analyzeData (clockFromFreq $ clockrate p) p g fret
  
-- | Return the rates of each of a list of spans
spansRates :: Clock -> [Span] -> V.Vector Time -> [Double]
spansRates clk spans times = 
  map (\b->(realToFrac (V.length b) + 0.5) / realDuration clk [b])
  $ filter (\b->V.length b > 20) -- Ensure we have enough statistics to make for good estimate
  $ spansPhotons spans times

-- | Return the average rate of a set of spans
spansRate :: Clock -> [Span] -> V.Vector Time -> Double
spansRate clk spans times = 
  let (n,t) = foldl' (\(n,t) a -> (n + realToFrac (V.length a) + 0.5, t + realDuration clk [a])) (0,0)
              $ filter (\b->V.length b > 2)
              $ spansPhotons spans times
  in n / t
            
-- | Background rate in Hz
backgroundRate :: Clock -> [Span] -> V.Vector Time  -> Double
backgroundRate clk bursts times =
  let range = (V.head times, V.last times)
      span_rates :: [Double]
      span_rates = spansRates clk (invertSpans range bursts) times
  --in mean $ V.fromList span_rates -- FIXME?
  --in realToFrac (V.length times) / realDuration clk [times]
  in spansRate clk (invertSpans range bursts) times
  
-- | Compute the crosstalk parameter alpha from donor-only spans
crosstalkParam :: Clock -> Fret (V.Vector Time) -> [Span] -> Double
crosstalkParam clk v dOnlySpans =
  mean $ V.fromList
  $ map (proximityRatio . fmap realToFrac)
  $ filter (\f->fretA f + fretD f > 10)
  $ burstCounts
  $ flipFrets
  $ fmap (spansPhotons dOnlySpans) v
         
spansFill :: [(RealTime, RealTime)] -> Plot RealTime Int    
spansFill spans = toPlot fill
        where coords = concat $ map f spans
                      where f (a,b) = [ (a, (0,0)), (a, (0,20))
                                      , (b, (0,20)), (b, (0,0))
                                      ]
              fill = plot_fillbetween_values ^= coords
                   $ plot_fillbetween_title  ^= "Detected bursts"
                   $ defaultPlotFillBetween

analyzeData :: Clock -> FretAnalysis -> Gamma -> Fret (V.Vector Time) -> IO ()
analyzeData clk p g fret = do 
  let range = (V.head $ fretA fret, V.last $ fretA fret)
  summary clk p "A" $ fretA fret
  summary clk p "D" $ fretD fret

  let duration = realDuration clk $ toList fret
  burstSpans <- fretBursts clk p fret
  let burstPhotons :: [Fret (V.Vector Time)]
      burstPhotons = filter (not . all V.null . toList)
                     $ flipFrets
                     $ fmap (spansPhotons burstSpans) fret

      bg_rate :: Fret Double
      bg_rate = fmap (backgroundRate clk burstSpans) fret
      --bg_rate = Fret 90 80
  let donorSpans = burstSpans `subtractSpans` dOnlyBursts clk p fret
  printf "Background rate: Donor=%1.1f, Acceptor=%1.1f\n" (fretD bg_rate) (fretA bg_rate)
  printf "Crosstalk: %1.2f\n" (crosstalkParam clk fret donorSpans)

  let burstDur :: [RealTime]
      burstDur = map (realDuration clk . toList) burstPhotons
      separate :: [Fret Double]
      separate = zipWith (\dur counts->(\n bg->n - bg*dur) <$> counts <*> bg_rate) burstDur
                 $ map (fmap realToFrac)
                 $ filter (\x->fretA x + fretD x > burst_size p)
                 $ burstCounts burstPhotons
  printf "Found %d bursts (%1.1f per second)\n"
    (length separate)
    (genericLength separate / duration)
  let a = spansFill $ map (\(a,b)->( timeToRealTime clk a
                                   , timeToRealTime clk b)
                          ) $ invertSpans range burstSpans
      layout = layout1_plots ^= map Right (a : plotFret clk fret 1e-2)
             $ (layout1_bottom_axis .> laxis_generate) ^= scaledAxis defaultLinearAxis (0,100)
             $ (layout1_right_axis .> laxis_generate) ^= scaledIntAxis defaultIntAxis (0,75)
             $ defaultLayout1
  renderableToPDFFile (toRenderable layout) 640 480 "bins.pdf"
  
  renderableToPNGFile (toRenderable
                       $ fretEffHist (n_bins p)
                       $ map (fretEfficiency g) separate
                      ) 640 480 (rootName p++"-fret_eff.png")
  writeFile (rootName p++"-fret_eff.txt")
    $ unlines $ map (show . fretEfficiency g) separate
  return ()
  
burstCounts :: [Fret (V.Vector Time)] -> [Fret Int]
burstCounts = map (fmap V.length)

testClock = clockFromFreq 1000000

testData :: Fret (V.Vector Time)
testData = Fret { fretA = V.generate 100000 (\i->fromIntegral $ i*1000)
                , fretD = V.generate 100000 (\i->fromIntegral $ i*1000)
                }

testData' :: Fret (V.Vector Time)
testData' = Fret { fretA=times e, fretD=times (1-e) }
          where e = 0.3
                times :: Double -> V.Vector Time
                times a = V.scanl1 (+)
                          $ V.concat $ replicate 10
                          $ V.replicate (round $ 1000 * a) (round $ realToFrac (freq testClock) * 1e-3 / a) V.++ V.singleton (freq testClock)

testMain = do
  let p = fretAnalysis { burst_thresh = 0.5
                       , burst_size = 0
                       }
  analyzeData testClock p 1 testData'
  
main = main'
