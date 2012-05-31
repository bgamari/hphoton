{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
    
import           Control.Applicative
import           Control.Monad (guard, liftM, when)
import           Control.Arrow (first, second)
import           Data.Accessor
import           Data.Foldable
import           Data.List (genericLength, stripPrefix)
import           Data.Maybe
import           Data.Traversable
import qualified Data.Vector.Unboxed as V

import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Plot.Histogram
import           Graphics.Rendering.Chart.Simple.Histogram
import           Data.Colour
import           Data.Colour.Names                 

import           HPhoton.Bin
import           HPhoton.Bin.Plot
import           HPhoton.BurstIdent.Bayes
import           HPhoton.BurstIdent.BinThreshold
import           HPhoton.FpgaTimetagger
import           HPhoton.Fret
import           HPhoton.Types
import           HPhoton.Utils

import           Control.Exception (catch, SomeException)                 
import           Numeric.MixtureModel.Beta as Beta
import           System.Random.MWC       
import           Data.Random       hiding (Gamma, gamma)

import           Prelude hiding (foldl1, concat, all, sum, catch, mapM_)
import           Statistics.Sample
import           System.Console.CmdArgs
import           System.Environment
import           Text.Printf

-- | A rate measured in real time
type Rate = Double

data BurstMode = Bayes
               | BayesCombined
               | BinThresh deriving (Show, Eq, Data, Typeable)

data FretAnalysis = FretAnalysis { clockrate :: Freq
                                 , n_bins :: Int
                                 , input :: [FilePath]
                                 , burst_mode :: BurstMode

                                 , bin_width :: RealTime
                                 , burst_thresh :: Double
                                 
                                 , beta_thresh :: Double
                                 , bg_rate :: Double
                                 , burst_size :: Int
                                 , burst_rate :: Double
                                 , prob_b :: Double
                                 , window :: Int

                                 , zero_fret :: Maybe ProxRatio
                                 , gamma :: Maybe Gamma
                                 }
                    deriving (Show, Eq, Data, Typeable)
                             
fretAnalysis = FretAnalysis { clockrate = round $ (128e6::Double) &= groupname "General" &= help "Timetagger clockrate (Hz)"
                            , n_bins = 20 &= groupname "General" &= help "Number of bins in efficiency histogram"
                            , input = def &= args &= typFile
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
                            , burst_rate = 3 &= groupname "Bayesian burst detection"
                                             &= help "Burst rate (multiple of avg.)"
                            , bg_rate = 1 &= groupname "Bayesian burst detection"
                                          &= help "Background rate (multiple of avg.)"
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
     
summarizeTimestamps :: Clock -> FretAnalysis -> String -> V.Vector Time -> IO ()
summarizeTimestamps clk p label photons =
  let len = realToFrac $ V.length photons :: Double
      dur = realDuration clk [photons]
  in printf "%-8s: %1.1e photons, %1.2e sec, %1.2e Hz\n" label len dur (len/dur)
     
burstSpans :: ModelParams -> Double -> V.Vector Time -> [Span]
burstSpans mp betaThresh times =
  let burstTimes = V.backpermute times
                   $ findBurstPhotons mp betaThresh
                   $ timesToInterarrivals times
  in V.toList $ compressSpans (3*mpTauBurst mp) burstTimes

donorSpans :: Clock -> FretAnalysis -> Fret (V.Vector Time) -> [Span]
donorSpans clk p d =
  let mp = modelParams clk p $ photonsRate clk (fretD d)
  in burstSpans mp (beta_thresh p) (fretD d)

acceptorSpans :: Clock -> FretAnalysis -> Fret (V.Vector Time) -> [Span]
acceptorSpans clk p d =
  let mp = modelParams clk p $ photonsRate clk (fretA d)
  in burstSpans mp (beta_thresh p) (fretA d)

modelParams :: Clock -> FretAnalysis -> Rate -> ModelParams
modelParams clk p avgRate =
  ModelParams { mpWindow = window p
              , mpProbB = prob_b p
              , mpTauBurst = round $ 1 / (avgRate * burst_rate p) / jiffy clk
              , mpTauBg = round $ 1 / (avgRate * bg_rate p) / jiffy clk
              }

fretEfficiency' :: Clock -> Fret (V.Vector Time) -> FretEff
fretEfficiency' clk times =               
  let binWidth = realTimeToTime clk 5e-3
      bins = fmap (bin binWidth) times :: Fret (V.Vector Int)
      prior = 5
      a :: V.Vector Double
      a = V.map (\(a,d)->realToFrac a / realToFrac (a+d))
          $ V.map (\(a,d)->(a+prior, d+prior))
          $ V.zip (fretA bins) (fretD bins)
      donorOnlyBins = fmap (V.backpermute $ V.findIndices (<0.4) a) bins
      fretBins = fmap (V.backpermute $ V.findIndices (>0.6) a) bins
  in 1 - mean (V.map realToFrac $ fretD donorOnlyBins) / mean (V.map realToFrac $ fretD fretBins)
  
photonsRate :: Clock -> V.Vector Time -> Rate
photonsRate clk times = realToFrac (V.length times) / realDuration clk [times]

fretBursts :: Clock -> FretAnalysis -> Fret (V.Vector Time) -> IO [Span]
fretBursts clk p@(FretAnalysis {burst_mode=BayesCombined}) d = do
  let combined = combineChannels $ toList d
      mp = modelParams clk p $ photonsRate clk combined
  putStrLn "Bayesian burst identification (combined) parameters:"
  print mp
  return $ burstSpans mp (beta_thresh p) combined

fretBursts clk p@(FretAnalysis {burst_mode=Bayes}) d = do
  let mp = modelParams clk p $ photonsRate clk (fretA d)
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
     
main' = do
  p <- cmdArgs fretAnalysis
  when (null $ input p) $ error "Need at least one input file"
  mapM_ (fileMain p) $ input p
               
stripSuffix :: String -> String -> String             
stripSuffix suffix = reverse . maybe (error "Invalid filename") id . stripPrefix (reverse suffix) . reverse

fileMain :: FretAnalysis -> FilePath -> IO ()
fileMain p input = do         
  printf "\nProcessing %s...\n" input
  recs <- readRecords input
  let fret = fmap (strobeTimes recs) fretChs
      rootName = stripSuffix ".timetag" input
  let g = case () of 
            _ | Just f <- zero_fret p -> gammaFromFret 0 f
            _ | Just g <- gamma p     -> g
            otherwise                 -> 1

  printf "Gamma: %f\n" g
  analyzeData rootName (clockFromFreq $ clockrate p) p g fret
  
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
  
correctBackground :: Rate -> RealTime -> Double -> Double
correctBackground rate dur n = n - dur*rate

correctFretBackground :: Fret Rate -> RealTime -> Fret Double -> Fret Double
correctFretBackground rate dur counts = correctBackground <$> rate <*> pure dur <*> counts

-- | Compute the crosstalk parameter alpha from donor-only spans
crosstalkParam :: Clock -> Fret (V.Vector Time) -> [Span] -> Double
crosstalkParam clk v dOnlySpans =
  mean $ V.fromList
  $ map (proximityRatio . fmap realToFrac)
  $ filter (\f->fretA f + fretD f > 10)
  $ burstCounts
  $ flipFrets
  $ fmap (spansPhotons dOnlySpans) v
  
correctCrosstalk :: Double -> Fret Double -> Fret Double
correctCrosstalk alpha counts =
  let n = alpha * (fretA counts + fretD counts)
  in Fret {fretA=subtract n, fretD=(+n)} <*> counts 

analyzeData :: String -> Clock -> FretAnalysis -> Gamma -> Fret (V.Vector Time) -> IO ()
analyzeData rootName clk p gamma fret = do 
  let range = (V.head $ fretA fret, V.last $ fretA fret)
  summarizeTimestamps clk p "A" $ fretA fret
  summarizeTimestamps clk p "D" $ fretD fret
  print $ fretEfficiency' clk fret

  let duration = realDuration clk $ toList fret
  burstSpans <- fretBursts clk p fret
  let burstPhotons :: [Fret (V.Vector Time)]
      burstPhotons = filter (not . all V.null . toList)
                     $ flipFrets
                     $ fmap (spansPhotons burstSpans) fret

      bg_rate :: Fret Rate
      bg_rate = fmap (backgroundRate clk burstSpans) fret
  printf "Background rate: Donor=%1.1f, Acceptor=%1.1f\n" (fretD bg_rate) (fretA bg_rate)
  let (mu,sigma) = meanVariance $ V.fromList $ map (realSpanDuration clk) burstSpans
  printf "Burst lengths: mu=%1.2e seconds, sigma=%1.2e seconds\n" mu sigma

  let dSpans = donorSpans clk p fret
      aSpans = acceptorSpans clk p fret
      dOnlySpans = burstSpans `subtractSpans` aSpans
      crosstalk = crosstalkParam clk fret dSpans
  printf "Crosstalk: %1.2f\n" crosstalk

  let burstDur :: [RealTime]
      burstDur = map (realDuration clk . toList) burstPhotons
      burstRates :: [Fret Rate]
      burstRates = map (correctCrosstalk crosstalk)
                 $ zipWith (correctFretBackground bg_rate) burstDur
                 $ map (fmap realToFrac)
                 $ filter (\x->fretA x + fretD x > burst_size p)
                 $ burstCounts burstPhotons

  printf "Found %d bursts (%1.1f per second)\n"
    (length burstRates)
    (genericLength burstRates / duration)
  writeFile (rootName++"-fret_eff.txt")
    $ unlines $ map (show . fretEfficiency gamma) burstRates
  
  let fretEffs = map (fretEfficiency gamma) burstRates
      fitFailed :: SomeException -> IO (Maybe ComponentParams)
      fitFailed _ = putStrLn "Fit Failed" >> return Nothing
  fitParams <- catch (Just `liftM` fitFretHist fretEffs) fitFailed
  let scale = realToFrac (length fretEffs) / realToFrac (n_bins p)
      layout = layout1_plots ^= [ Left $ plotFretHist (n_bins p) fretEffs ]
                                ++ maybe [] (map Left . plotFit scale) fitParams
               $ layout1_title ^= rootName
               $ defaultLayout1
  renderableToSVGFile (toRenderable layout) 640 480 (rootName++"-fret_eff.svg")

  --plotFretAnalysis clk gamma p fret (zip burstSpans burstRates)
  return ()

fitFretHist :: [FretEff] -> IO ComponentParams
fitFretHist fretEffs = do
  mwc <- create
  fitParams <- runRVar (runFit 250 $ V.fromList $ filter (\x->x>0 && x<1) fretEffs) mwc
  putStrLn $ unlines 
           $ map (\(w,p)->let (mu,sigma) = paramToMoments p
                          in printf "weight=%1.2f, mu=%1.2f, sigma^2=%1.2f" w mu sigma
                 ) $ V.toList fitParams
  return fitParams
  
replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f

priors :: [(Weight, BetaParam)]       
priors = [ (0.5, paramFromMoments (0.1, 0.01))
         , (0.5, paramFromMoments (0.9, 0.01))
         ]

runFit :: Int -> Samples -> RVar ComponentParams
runFit niter samples = do
    a0 <- updateAssignments' samples (V.fromList priors)
    a <- replicateM' 500 (updateAssignments samples 2) a0
    return $ estimateWeights a $ paramsFromAssignments samples 2 a

plotFit :: Double -> ComponentParams -> [Plot FretEff Double]
plotFit scale fitParams =
    [ functionPlot 1000 (0.01,0.99) $ \x->scale * dist x
    , toPlot
      $ plot_annotation_values ^= [(0,1,label)]
      $ defaultPlotAnnotation
    ]
    where dist x = sum $ map (\(w,p)->w * realToFrac (betaProb p x)) $ V.toList fitParams
          label = unlines 
                  $ map (\(w,p)->let (mu,sigma) = paramToMoments p
                                 in printf "weight=%1.2f, mu=%1.2f, sigma^2=%1.2f" w mu sigma
                        ) $ V.toList fitParams

functionPlot :: (RealFrac x, Enum x) => Int -> (x, x) -> (x -> y) -> Plot x y
functionPlot n (a,b) f =
  let xs = [a,a+(b-a)/realToFrac n..b]
  in toPlot $ plot_lines_values ^= [map (\x->(x,f x)) xs]
            $ plot_lines_style .> line_color ^= opaque red
            $ defaultPlotLines

plotFretHist :: Int -> [FretEff] -> Plot FretEff Double
plotFretHist nbins fretEffs =
    plotFloatHist
    $ plot_hist_values ^= [fretEffs]
    $ plot_hist_range  ^= Just (-0.1, 1.1)
    $ plot_hist_bins   ^= nbins
    $ defaultPlotHist

spansFill :: Int -> String -> [(RealTime, RealTime)] -> Plot RealTime Int    
spansFill maxY title spans = toPlot fill
        where coords = concat $ map f spans
                      where f (a,b) = [ (a, (0,0)), (a, (0,maxY))
                                      , (b, (0,maxY)), (b, (0,0))
                                      ]
              fill = plot_fillbetween_values ^= coords
                   $ plot_fillbetween_title  ^= title
                   $ defaultPlotFillBetween

plotFretAnalysis :: String -> Clock -> Gamma -> FretAnalysis -> Fret (V.Vector Time)
                 -> [(Span, Fret Rate)] -> IO ()
plotFretAnalysis rootName clk gamma p times bursts = do
  let (burstSpans, burstRates) = unzip bursts
      a = spansFill 20 "Bursts" $ map (\(a,b)->( timeToRealTime clk a
                                               , timeToRealTime clk b)
                                      ) burstSpans
      b = spansFill 30 "Donor only" $ map (\(a,b)->( timeToRealTime clk a
                                                   , timeToRealTime clk b)
                                          ) burstSpans
      layout :: Layout1 RealTime Int
      layout = layout1_plots ^= map Left (a : b : plotFret clk times 1e-2)
             $ (layout1_bottom_axis .> laxis_generate) ^= scaledAxis defaultLinearAxis (30,60)
             $ (layout1_left_axis .> laxis_generate) ^= scaledIntAxis defaultIntAxis (0,75)
             $ defaultLayout1

      c = plot_points_values ^= map (\((s,e), f)->(timeToRealTime clk s, proximityRatio f)) bursts
          $ plot_points_style ^= plusses 2 0.1 (opaque orange)
          $ plot_points_title ^= "Burst FRET efficiency"
          $ defaultPlotPoints
      l2 :: Layout1 RealTime FretEff
      l2 = layout1_plots ^= [ Left $ plotFretEff clk times 1e-2 1
                            , Left $ toPlot c]
         $ (layout1_bottom_axis .> laxis_generate) ^= scaledAxis defaultLinearAxis (30,60)
         $ (layout1_left_axis .> laxis_generate) ^= scaledAxis defaultLinearAxis (0,1)
         $ defaultLayout1

  renderableToPDFFile (renderLayout1sStacked [ withAnyOrdinate layout
                                             , withAnyOrdinate l2]
                      )
                      5000 600 (rootName++"-bins.pdf")
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
  analyzeData "test" testClock p 1 testData'
  
main = main'
