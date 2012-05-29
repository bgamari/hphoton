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
               
stripSuffix :: String -> String -> String             
stripSuffix suffix = reverse . maybe (error "Invalid filename") id . stripPrefix (reverse suffix) . reverse

rootName :: FretAnalysis -> String
rootName = stripSuffix ".timetag" . maybe (error "Need filename") id . input

fretChs = Fret { fretA = Ch1
               , fretD = Ch0
               }
     
summary :: FretAnalysis -> String -> Clocked (V.Vector Time) -> IO ()
summary p label photons =
  let len = realToFrac $ V.length (unClocked photons) :: Double
      dur = realDuration $ fmap (:[]) photons
  in printf "%-8s: %1.1e photons, %1.2e sec, %1.2e Hz\n" label len dur (len/dur)
     
burstSpans :: ModelParams -> Double -> V.Vector Time -> [Span]
burstSpans mp betaThresh times =
  let burstTimes = V.backpermute times
                   $ findBurstPhotons mp betaThresh
                   $ timesToInterarrivals times
  in V.toList $ compressSpans (10*mpTauBurst mp) burstTimes

dOnlyBursts :: FretAnalysis -> Clocked (Fret (V.Vector Time)) -> [Span]
dOnlyBursts p d =
  let jiffy = realToFrac $ clockrate p
      mp = ModelParams { mpWindow = window p
                       , mpProbB = prob_b p
                       , mpTauBurst = round $ 1 / 140 * jiffy
                       , mpTauBg = round $ 1 / 70 * jiffy
                       }
  in burstSpans mp (beta_thresh p) (fretA $ unClocked d)

fretBursts :: FretAnalysis -> Clocked (Fret (V.Vector Time)) -> IO [Span]
fretBursts p@(FretAnalysis {burst_mode=Bayes}) d = do
  let jiffy = realToFrac $ clockrate p
      mp = ModelParams { mpWindow = window p
                       , mpProbB = prob_b p
                       , mpTauBurst = round $ 1 / burst_rate p * jiffy
                       , mpTauBg = round $ 1 / bg_rate p * jiffy
                       }
      combined = combineChannels $ toList $ unClocked d
  putStrLn "Bayesian burst identification parameters:"
  print mp
  return $ burstSpans mp (beta_thresh p) combined

fretBursts p@(FretAnalysis {burst_mode=BinThresh}) d = do
  let combined = combineChannels $ toList $ unClocked d
      binWidthTicks = round $ 1e-3*bin_width p / jiffy d
      len = realToFrac $ V.length combined :: Double
      dur = photonsDuration (jiffy d) combined
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
  let fret = Clocked (clockrate p)
             $ fmap (strobeTimes recs) fretChs
  let g = case () of 
            _ | Just f <- zero_fret p -> gammaFromFret 0 f
            _ | Just g <- gamma p     -> g
            otherwise                 -> 1

  printf "Gamma: %f\n" g
  analyzeData p g fret
  
-- | Return the rates of a set of spans
spanRates :: [Span] -> Clocked (V.Vector Time) -> [Double]
spanRates spans times = 
  map (\b->(realToFrac (V.length b) + 0.5) / realDuration (clockedLike times [b]))
  $ filter (\b->V.length b > 20) -- Ensure we have enough statistics to make for good estimate
  $ unClocked
  $ fmap (flip spansPhotons $ spans) times
            
-- | Background rate in Hz
backgroundRate :: Clocked (V.Vector Time) -> [Span] -> Double
backgroundRate times bursts =
  let range = (V.head $ unClocked times, V.last $ unClocked times)
      span_rates :: [Double]
      span_rates = spanRates (invertSpans range bursts) times
  --in mean $ V.fromList span_rates -- FIXME?
  in realToFrac (V.length $ unClocked times) / realDuration (fmap (:[]) times)
  
crosstalkParam :: Clock (V.Vector Time) -> [Span] -> Double
crosstalkParam v dOnlySpans =
  map proximityRatio sep
  $ separateBursts
  $ fmap (fmap (flip spansPhotons $ dOnlySpans)) v
         
spansFill :: [(RealTime, RealTime)] -> Plot RealTime Int    
spansFill spans = toPlot fill
        where coords = concat $ map f spans
                      where f (a,b) = [ (a, (0,0)), (a, (0,20))
                                      , (b, (0,20)), (b, (0,0))
                                      ]
              fill = plot_fillbetween_values ^= coords
                   $ plot_fillbetween_title  ^= "Detected bursts"
                   $ defaultPlotFillBetween

analyzeData :: FretAnalysis -> Gamma -> Clocked (Fret (V.Vector Time)) -> IO ()
analyzeData p g fret = do 
  let range = (V.head $ fretA $ unClocked fret, V.last $ fretA $ unClocked fret)
  summary p "A" $ fretA $ sequenceA fret
  summary p "D" $ fretD $ sequenceA fret

  let duration = realDuration $ fmap toList fret
  spans <- fretBursts p fret
  let burstPhotons :: Clocked [Fret (V.Vector Time)]
      burstPhotons = fmap (filter (not . all V.null . toList)
                          . flipFret
                          . fmap (flip spansPhotons $ spans)
                          ) fret

      bg_rate :: Fret Double
      --bg_rate = fmap (flip backgroundRate $ spans) $ sequenceA fret
      bg_rate = Fret 90 80
  printf "Background rate: Donor=%1.1f, Acceptor=%1.1f\n" (fretD bg_rate) (fretA bg_rate)

  let donorSpans = spans `subtractSpans` dOnlyBursts p fret
  --print $ fmap (map V.length . (flip spansPhotons $ donorSpans)) $ unClocked fret
  
  let burstDur :: [RealTime]
      burstDur = map (realDuration . clockedLike fret . toList)
                 $ unClocked burstPhotons
      separate :: [Fret Double]
      separate = zipWith (\dur counts->(\n bg->n - bg*dur) <$> counts <*> bg_rate) burstDur
                 $ map (fmap realToFrac)
                 $ filter (\x->fretA x + fretD x > burst_size p)
                 $ burstCounts $ unClocked burstPhotons
  printf "Found %d bursts (%1.1f per second)\n"
    (length separate)
    (genericLength separate / duration)
  let a = spansFill $ map (\(a,b)->( timeToRealTime $ Clocked (freq fret) a
                                   , timeToRealTime $ Clocked (freq fret) b)
                          ) $ invertSpans range spans
      layout = layout1_plots ^= map Right (a : plotFret fret 1e-2)
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

flipFret :: Fret [a] -> [Fret a]            
flipFret (Fret a b) = zipWith Fret a b
 
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
