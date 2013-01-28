{-# LANGUAGE PatternGuards      #-}

import           Prelude hiding (all, catch, concat, foldl1, mapM_, sum)

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad (guard, liftM, when)
import           Data.Foldable
import           Data.List (genericLength, stripPrefix, partition)
import           Text.Printf

import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import qualified Data.Vector.Unboxed as V

import           HPhoton.Bin
import           HPhoton.Bin.Plot
import           HPhoton.BurstIdent.Bayes
import           HPhoton.BurstIdent.BinThreshold
import           HPhoton.FpgaTimetagger
import           HPhoton.Fret
import           HPhoton.Types
import           HPhoton.Utils

import           Control.Exception (SomeException, catch)
import           Options.Applicative

import           Data.Random hiding (Gamma, gamma)
import           Numeric.MixtureModel.Beta as Beta hiding (Prob)
import           System.Random.MWC
import           Statistics.Sample

import           Data.Accessor
import           Data.Colour
import           Data.Colour.Names
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Plot.Histogram
import           Graphics.Rendering.Chart.Simple.Histogram

stripSuffix :: String -> String -> String
stripSuffix suffix =
    reverse . maybe (error "Invalid filename") id . stripPrefix (reverse suffix) . reverse

-- | A rate measured in real time
type Rate = Double

-- | The Crosstalk parameter, alpha
type CrosstalkParam = Double

data BurstIdentChannel = SingleChannel FretChannel
                       | CombinedChannels
                       deriving (Show, Eq)

data BurstMode = Bayes { bayesWindow       :: Int
                       , bayesBetaThresh   :: Double
                       , bayesProbB        :: Prob
                       , bayesBurstRateFactor :: Double
                       , bayesBgRateFactor :: Double
                       , bayesChannel      :: BurstIdentChannel
                       }
               | BinThresh RealTime BinThreshold -- ^ BinThresh binWidth thresholdType
               deriving (Show, Eq)

data FretAnalysis = FretAnalysis { clockrate :: Freq
                                 , n_bins :: Int
                                 , input :: [FilePath]
                                 , burst_size :: Int
                                 , burst_mode :: BurstMode

                                 , crosstalk :: FretEff
                                 , gamma :: Bool

                                 , fit_ncomps :: Int
                                 }
                    deriving (Show, Eq)

burstMode :: Parser BurstMode
burstMode = bayes <|> binThresh
  where bayes = Bayes
                <$> option ( long "bayes-window" <> value 10
                          <> help "Window size" )
                <*> option ( long "bayes-beta-thresh" <> value 2
                          <> help "Threshold on beta" )
                <*> option ( long "bayes-prob-b" <> value 0.01
                          <> help "Burst probability" )
                <*> option ( long "bayes-burst-rate" <> value 3
                          <> help "Burst rate factor" )
                <*> option ( long "bayes-bg-rate" <> value 1
                          <> help "Background rate factor" )
                <*> ( ((const CombinedChannels) <$> switch
                          ( long "bayes-combined"
                         <> help "Run Bayesian burst detection on combined"
                          )
                      )
                      <|>
                      (SingleChannel <$> nullOption
                           ( long "bayes-single" <> reader (const $ pure Donor)
                          <> help "Run Bayesian burst detection on a single channel"
                           )
                      )
                    )
                <*  switch ( long "bayes" <> help "Use Bayesian burst detection" )
        binThresh = BinThresh
                    <$> option ( long "bin-width" <> value 10
                              <> help "Binning bin width in milliseconds" )
                    <*> nullOption ( long "burst-thresh" <> reader (pure . MultMeanThresh . read)
                                  <> value (MultMeanThresh 2)
                                  <> help "Burst count threshold" )
                    <*  switch ( long "bin-thresh"
                              <> help "Use binning/thresholding for burst detection" )

fretAnalysis :: Parser FretAnalysis
fretAnalysis = FretAnalysis
    <$> option ( long "clockrate" <> short 'c'
              <> value (round $ (128e6::Double))
              <> help "Timetagger clockrate (Hz)"
               )
    <*> option ( long "nbins" <> short 'n'
              <> value 50
              <> help "Number of bins in efficiency histogram"
               )
    <*> arguments1 Just ( help "Input files" )
    <*> option ( long "burst-size" <> short 's'
              <> value 10
              <> help "Minimum burst size in photons"
               )
    <*> burstMode
    <*> option ( long "crosstalk" <> value 0
              <> help "Measured efficiency for zero FRET to compute crosstalk correction"
               )
    <*> switch ( long "gamma"
              <> help "Enable gamma correction from donor-only signal"
               )
    <*> option ( long "fit" <> value 2
              <> help "Number of Beta components to fit histogram against"
               )

fretChs = Fret { fretA = Ch1, fretD = Ch0 }

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

fretBursts :: Clock -> BurstMode -> Fret (V.Vector Time) -> [Span]
fretBursts clk b@(Bayes {}) d =
    let channel = case bayesChannel b of
                       SingleChannel c  -> getFretChannel d c
                       CombinedChannels -> combineChannels $ toList d
        avgRate = photonsRate clk channel
        mp = ModelParams { mpWindow = bayesWindow b
                         , mpProbB = bayesProbB b
                         , mpTauBurst = round $ 1 / (avgRate * bayesBurstRateFactor b) / jiffy clk
                         , mpTauBg = round $ 1 / (avgRate * bayesBgRateFactor b) / jiffy clk
                         }
    in burstSpans mp (bayesBetaThresh b) channel

fretBursts clk (BinThresh binWidth thresh) d =
    let combined = combineChannels $ toList d
        binWidthTicks = round $ 1e-3*binWidth / jiffy clk
        len = realToFrac $ V.length combined :: Double
    in V.toList $ findBursts binWidthTicks thresh combined

-- | Return the rates of each of a list of spans
spansRates :: Clock -> [Span] -> V.Vector Time -> [Rate]
spansRates clk spans times =
    map (\b->(realToFrac $ V.length b) / realDuration clk [b])
    $ filter (\b->V.length b > 20) -- Ensure we have enough statistics for a good estimate
    $ spansPhotons spans times

-- | Return the average rate of a set of spans
spansRate :: Clock -> [Span] -> V.Vector Time -> Rate
spansRate clk spans times =
    let (n,t) = foldl' (\(n,t) a -> (n + realToFrac (V.length a) + 0.5, t + realDuration clk [a])) (0,0)
                $ filter (\b->V.length b > 2)
                $ spansPhotons spans times
    in n / t

-- | Background rate in Hz
backgroundRate :: Clock -> [Span] -> V.Vector Time -> Rate
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
correctFretBackground rate dur counts =
    correctBackground <$> rate <*> pure dur <*> counts

-- | Compute the crosstalk parameter alpha from donor-only spans
crosstalkParam :: Clock -> Fret (V.Vector Time) -> [Span] -> CrosstalkParam
crosstalkParam clk v dOnlySpans =
    mean $ V.fromList
    $ map (proximityRatio . fmap realToFrac)
    $ filter (\f->fretA f + fretD f > 10)
    $ burstCounts
    $ flipFrets
    $ fmap (spansPhotons dOnlySpans) v

correctCrosstalk :: CrosstalkParam -> Fret Double -> Fret Double
correctCrosstalk alpha counts =
    let n = alpha * (fretA counts + fretD counts)
    in Fret {fretA=subtract n, fretD=(+n)} <*> counts

-- | Compute gamma from change in intensity between donor-only and donor-acceptor
gammaFromDelta :: CrosstalkParam -> Fret Rate -> Fret Rate -> Gamma
gammaFromDelta crosstalk donorOnly donorAcceptor =
    crosstalk - fretA deltaI / fretD deltaI
    where deltaI = (-) <$> donorAcceptor <*> donorOnly

fac :: Int -> Int
fac 0 = 1
fac n = n*fac (n-1)

poissonLikelihood :: Int -> Int -> Prob
poissonLikelihood lambda k =
    realToFrac (lambda^k) * exp (realToFrac $ -k) / realToFrac (fac lambda)

analyzeData :: String -> Clock -> FretAnalysis -> Fret (V.Vector Time) -> IO ()
analyzeData rootName clk p fret = do
    let range = (V.head $ fretA fret, V.last $ fretA fret)
    summarizeTimestamps clk p "A" $ fretA fret
    summarizeTimestamps clk p "D" $ fretD fret

    let duration = realDuration clk $ toList fret
        burstSpans = filter (\span->realSpanDuration clk span > 1e-4)
                     $ fretBursts clk (burst_mode p) fret
        burstPhotons = filter (not . all V.null . toList)
                       $ flipFrets
                       $ fmap (spansPhotons burstSpans)
                       $ fret

    writeFile (rootName++"-burst-length.txt")
        $ unlines $ map (show . realSpanDuration clk) burstSpans

    let bgRate = backgroundRate clk burstSpans <$> fret :: Fret Rate
    printf "Background rate: Donor=%1.1f, Acceptor=%1.1f\n" (fretD bgRate) (fretA bgRate)
    let (mu,sigma) = meanVariance $ V.fromList $ map (realSpanDuration clk) burstSpans
    printf "Burst lengths: mu=%1.2e seconds, sigma=%1.2e seconds\n" mu sigma
    printf "Crosstalk: %1.2f\n" (crosstalk p)

    let bursts :: [(Time, Fret Int)]
        bursts = filter ((\x->fretA x + fretD x > burst_size p) . snd)
                 $ zip (map spanDuration burstSpans)
                       (flipFrets $ spansCounts burstSpans <$> fret)
        burstRates :: [Fret Rate]
        burstRates = map (correctCrosstalk $ crosstalk p)
                   $ map (\(dur,counts)->(/timeToRealTime clk dur) <$> counts)
                   $ map (\(dur,counts)->(dur, correctFretBackground bgRate (timeToRealTime clk dur) counts))
                   $ map (second (fmap realToFrac))
                   $ bursts

    let (daRates,dRates) = partition (\span->proximityRatio span > 0.35) -- FIXME
                           $ burstRates
        daRate = fmap (max 0 . mean . V.fromList) $ unflipFrets daRates
        dRate = fmap (max 0 . mean . V.fromList) $ unflipFrets dRates
        _gamma = case gamma p of 
                     True  -> gammaFromDelta (crosstalk p) dRate daRate
                     False -> 1
    printf "Gamma=%f\n" _gamma

    let fretEffs = map (fretEfficiency _gamma) burstRates
    printf "Found %d bursts (%1.1f per second)\n"
      (length burstRates)
      (genericLength burstRates / duration)
    writeFile (rootName++"-fret_eff.txt")
      $ unlines $ map show fretEffs

    plotFretAnalysis rootName clk p fret (zip burstSpans burstRates)

    let fitFailed :: SomeException -> IO (Maybe ComponentParams)
        fitFailed _ = putStrLn "Fit Failed" >> return Nothing
    fitParams <- catch (Just `liftM` fitFretHist (fit_ncomps p) fretEffs) fitFailed
    let formatFit params = let formatComp (w,p) = let (mu,sigma) = paramToMoments p
                                                  in printf "%1.3e\t%1.3e\t%1.3e" w mu sigma
                               header = "# weight\tmu\tsigma"
                           in unlines $ header:(map formatComp $ V.toList params)
    writeFile (rootName++"-fit.txt") $ maybe "" formatFit fitParams


    let scale = realToFrac (length fretEffs) / realToFrac (n_bins p)
        layout = layout1_plots ^= [ Left $ plotFretHist (n_bins p) fretEffs ]
                                  ++ maybe [] (map Left . plotFit scale) fitParams
                 $ layout1_title ^= rootName
                 $ defaultLayout1
    renderableToSVGFile (toRenderable layout) 640 480 (rootName++"-fret_eff.svg")

    return ()

fitFretHist :: Int -> [FretEff] -> IO ComponentParams
fitFretHist ncomps fretEffs = do
    mwc <- create
    fitParams <- runRVar (runFit ncomps 250
                          $ V.fromList
                          $ filter (\x->x>0 && x<1) fretEffs
                         ) mwc
    putStrLn $ unlines
             $ map (\(w,p)->let (mu,sigma) = paramToMoments p
                            in printf "weight=%1.2f, mu=%1.2f, sigma^2=%1.2f" w mu sigma
                   ) $ V.toList fitParams
    return fitParams

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f

priors :: Int -> [(Weight, BetaParam)]
priors ncomps = map component [1..ncomps]
    where component i = ( 1 / realToFrac ncomps
                        , paramFromMoments (realToFrac i/(realToFrac ncomps+2), 0.01)
                        )

runFit :: Int -> Int -> Samples -> RVar ComponentParams
runFit ncomps niter samples = do
    a0 <- updateAssignments' samples (V.fromList $ priors ncomps)
    a <- replicateM' 500 (updateAssignments samples ncomps) a0
    return $ estimateWeights a $ paramsFromAssignments samples ncomps a

plotFit :: Double -> ComponentParams -> [Plot FretEff Double]
plotFit scale fitParams =
    [ functionPlot 1000 (0.01,0.99) $ \x->scale * dist x
    , toPlot
      $ plot_annotation_values ^= [(0.9,10,label)]
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
    histToPlot
    $ plot_hist_values ^= fretEffs
    $ plot_hist_range  ^= Just (-0.1, 1.1)
    $ plot_hist_bins   ^= nbins
    $ defaultFloatPlotHist

spansFill :: Int -> String -> [(RealTime, RealTime)] -> Plot RealTime Int
spansFill maxY title spans = toPlot fill
    where coords = concat $ map f spans
                   where f (a,b) = [ (a, (0,0)), (a, (0,maxY))
                                   , (b, (0,maxY)), (b, (0,0))
                                   ]
          fill = plot_fillbetween_values ^= coords
               $ plot_fillbetween_title  ^= title
               $ defaultPlotFillBetween

plotFretAnalysis :: String -> Clock -> FretAnalysis -> Fret (V.Vector Time)
                 -> [(Span, Fret Rate)] -> IO ()
plotFretAnalysis rootName clk p times bursts = do
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
           $ layout1_title ^= rootName
           $ defaultLayout1
  
    renderableToPDFFile (renderLayout1sStacked [ withAnyOrdinate layout
                                               , withAnyOrdinate l2]
                        )
                        4000 600 (rootName++"-bins.pdf")
    return ()

burstCounts :: [Fret (V.Vector Time)] -> [Fret Int]
burstCounts = map (fmap V.length)

main = do
    let opts = info (helper <*> fretAnalysis)
                    ( fullDesc 
                   <> progDesc "FRET analysis"
                   <> header "fret-analysis - A simple FRET analysis" )
    p <- execParser opts
    when (null $ input p) $ error "Need at least one input file"
    mapM_ (fileMain p) $ input p

fileMain :: FretAnalysis -> FilePath -> IO ()
fileMain p input = do
    printf "\nProcessing %s...\n" input
    recs <- readRecords input

    let fret = fmap (strobeTimes recs) fretChs
        rootName = stripSuffix ".timetag" input
    analyzeData rootName (clockFromFreq $ clockrate p) p fret
  
