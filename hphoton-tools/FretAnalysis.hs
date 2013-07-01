{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.List (partition, intercalate, zipWith4)
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.IO.Class

import           System.IO
import           System.Directory (doesFileExist, createDirectoryIfMissing)
import           System.FilePath
import           Control.Proxy as P
import qualified Control.Proxy.ByteString as PBS
import           Control.Proxy.Vector

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB

import           HPhoton.Bin
import           HPhoton.FpgaTimetagger
import           HPhoton.FpgaTimetagger.Pipe
import           HPhoton.Fret
import           HPhoton.Types
import qualified Moments as M

import           Numeric
import           HtmlLog
import           FretFit
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Printf

import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Plot.Histogram
import           Data.Colour
import           Data.Colour.SRGB (sRGB)
import           Data.Colour.RGBSpace (uncurryRGB)
import           Data.Colour.RGBSpace.HSV (hsv)
import           Data.Colour.Names

import           Numeric.MixtureModel.Beta as Beta hiding (Prob)
import           Numeric.SpecFunctions (logFactorial)
import           Numeric.Log hiding (sum)
import           Statistics.Sample
import           Statistics.Resampling

import           Options.Applicative

type Rate = Double

data FretAnalysis = FretAnalysis { clockrate :: Freq
                                 , input :: [FilePath]
                                 , binWidth :: Double
                                 , burstSize :: Int
                                 , upperThresh :: Maybe Int
                                 , nbins :: Int
                                 , dOnlyFile :: Maybe FilePath
                                 , gamma :: Maybe Double
                                 , crosstalk :: Maybe Crosstalk
                                 , outputDir :: FilePath
                                 , fitComps :: Int
                                 , dOnlyCriterion :: DOnlyPartitioning
                                 }
                    deriving (Show, Eq)

fretAnalysis :: Parser FretAnalysis
fretAnalysis = FretAnalysis
    <$> option ( long "clockrate" <> short 'c'
              <> value (round $ (128e6::Double))
              <> metavar "FREQ"
              <> help "Timetagger clockrate (Hz)"
               )
    <*> arguments1 Just ( help "Input files" <> action "file" )
    <*> option ( long "bin-width" <> short 'w'
              <> value 1e-3
              <> metavar "TIME"
              <> help "Width of temporal bins"
               )
    <*> option ( long "burst-size" <> short 's'
              <> value 500
              <> metavar "N"
              <> help "Minimum burst rate in counts per bin"
               )
    <*> option ( long "upper-thresh" <> short 'u'
              <> value Nothing
              <> metavar "N"
              <> help "Throw out bins with greater than N photons"
               )
    <*> option ( long "nbins" <> short 'n'
              <> value 50
              <> metavar "N"
              <> help "Number of bins in the FRET efficiency histogram"
               )
    <*> nullOption ( long "donly-file" <> short 'D'
                  <> value Nothing
                  <> reader (pure . Just)
                  <> help "Donor only file to use for gamma and crosstalk estimation; uses donor-only population of current file by default"
                   )
    <*> nullOption ( long "gamma" <> short 'g'
                  <> value (Just 1)
                  <> reader (\s->if s == "auto"
                                 then pure $ Nothing
                                 else fmap Just $ auto s
                            )
                  <> metavar "[N]"
                  <> help "Gamma correct resulting histogram. If 'auto' is given, gamma will be estimated from the slope of the Donor-Acceptor population."
                   )
    <*> option ( long "crosstalk" <> short 't'
              <> value (Just 0)
              <> reader (\s->if s == "auto"
                             then pure Nothing
                             else Just <$> auto s
                        )
              <> metavar "[E]"
              <> help "Use crosstalk correction"
               )
    <*> strOption ( long "output" <> short 'o'
              <> value "." <> metavar "DIR"
              <> help "Directory in which to place output files"
               )
    <*> option ( long "fit-comps" <> short 'f'
              <> value 1 <> metavar "N"
              <> help "Number of Beta fit components"
               )
    <*> dOnlyPartitioning

dOnlyPartitioning :: Parser DOnlyPartitioning
dOnlyPartitioning = eThresh <|> fitOdds
  where eThresh = EThresh
            <$> option ( long "donly-thresh" <> short 'd'
                      <> value 0.2 <> metavar "E"
                      <> help "FRET efficiency cut-off for donor-only population"
                       )
        fitOdds = FitOdds
            <$> option ( long "donly-fit-comps"
                      <> value 2 <> metavar "O"
                      <> help "Number of components to fit"
                       )

main = do
    let opts = info (helper <*> fretAnalysis)
                    ( fullDesc
                   <> progDesc "FRET analysis"
                    )
    p <- execParser opts
    createDirectoryIfMissing True (outputDir p)
    forM_ (input p) $ goFile p

-- | Strict foldMap
foldMap' :: (F.Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap' f = F.foldl' (\m a->mappend m $! f a) mempty

data DOnlyPartitioning = EThresh ProxRatio
                       | FitOdds Int
                       deriving (Show, Eq)

partitionDOnly :: DOnlyPartitioning -> [Fret Double] -> IO ([Fret Double], [Fret Double])
partitionDOnly (EThresh e) bins =
    return $ partition (\b->proximityRatio b < e) bins
partitionDOnly (FitOdds nComps) bins = do
    fitParams <- maybe (error "Failed to fit for D-only paritioning") id
                 <$> fitFret 200 nComps (map proximityRatio bins)
    let dOnlyOdds b = let e = proximityRatio b
                          probComp (w,c) e = realToFrac w * betaProb c e
                      in probComp (V.head fitParams) e / probComp (V.last fitParams) e
    return ( filter (\b->dOnlyOdds b > 2) bins
           , filter (\b->dOnlyOdds b < 1/2) bins
           )

readFretBins :: Fret Channel -> Time -> FilePath -> IO [Fret Double]
readFretBins fretChannels binTime fname = do
    recs <- liftIO $ withFile fname ReadMode $ \fIn->
        runProxy $ runToVectorK $   PBS.readHandleS fIn
                                >-> decodeRecordsP
                                >-> dropD 1024
                                >-> filterDeltasP
                                >-> toVectorD
    let times = unwrapTimes 0xfffffffff . strobeTimes recs <$> fretChannels :: Fret (VU.Vector Time)
        bins = map (fmap fromIntegral) $ binMany binTime times
    return bins
    
getFretBins :: FilePath -> Fret Channel -> Time -> FilePath -> HtmlLogT IO [Fret Double]
getFretBins outputRoot fretChannels binTime fname = do
    bins <- liftIO $ readFretBins fretChannels binTime fname
    liftIO $ renderableToSVGFile
        (layoutThese plotBinTimeseries (Fret "Acceptor" "Donor") $ T.sequenceA bins)
        500 500 (outputRoot++"-bins.svg")
    return bins

summarizeCountStatistics :: Monad m => [Fret Double] -> [Fret Double] -> HtmlLogT m ()
summarizeCountStatistics bgBins fgBins = do                         
    let fgCountMoments = foldMap' (fmap M.sample) fgBins
        bgCountMoments = foldMap' (fmap M.sample) bgBins
    tellLog 10 $ H.section $ do
        H.h2 "Count statistics"
        let total = getSum <$> foldMap' (fmap Sum) fgBins
            rows :: H.ToMarkup a => [[a]] -> H.Html
            rows = mapM_ (H.tr . mapM_ (H.td . H.toHtml))
            fretRows :: Fret [String] -> H.Html
            fretRows fret = rows [ ["Acceptor"] ++ fretA fret
                                 , ["Donor"] ++ fretD fret
                                 ]
        H.table $ do
            H.tr $ H.th "Total counts"
            fretRows $ fmap (\x->[showFFloat (Just 2) x ""]) total

            H.tr $ H.th "Foreground counts"
            H.tr $ mapM_ H.th ["", "mean", "variance"]
            fretRows $ fmap (\m->[ showFFloat (Just 2) (M.mean m) ""
                                 , showFFloat (Just 2) (M.variance m) ""
                                 ]) fgCountMoments
            rows [ ["Number of foreground bins", show $ length fgBins] ]

            H.tr $ H.th "Background counts"
            H.tr $ mapM_ H.th ["", "mean", "variance"]
            fretRows $ fmap (\m->[ showFFloat (Just 2) (M.mean m) ""
                                 , showFFloat (Just 2) (M.variance m) ""
                                 ]) bgCountMoments
            rows [ ["Number of background bins", show $ length bgBins] ]

goFile :: FretAnalysis -> FilePath -> IO ()
goFile p fname = writeHtmlLogT (fname++".html") $ do
    liftIO $ putStrLn fname
    tellLog 0 $ H.h1 $ H.toHtml fname
    tellLog 100 $ H.section $ do H.h2 "Analysis parameters"
                                 H.code $ H.toHtml $ show p
    let outputRoot = replaceDirectory fname (outputDir p)

    let fretChannels = Fret Ch1 Ch0
    let clk = clockFromFreq $ clockrate p
    (fgBins,bgBins) <- partition (\a->F.sum a > realToFrac (burstSize p))
                   <$> getFretBins outputRoot fretChannels (realTimeToTime clk (binWidth p)) fname
                    :: HtmlLogT IO ([Fret Double], [Fret Double])

    liftIO $ let names = Fret "acceptor" "donor"
                 colours = flip withOpacity 0.5 <$> Fret red green
                 layout = layoutCountingHist fname 100 names colours (fmap V.fromList $ T.sequenceA fgBins)
             in renderableToSVGFile layout 640 480 (outputRoot++"-pch.svg")
    tellLog 15 $ H.section $ do
        H.h2 "Photon Counting Histogram"
        H.img H.! HA.src (H.toValue $ fname++"-pch.svg")
              H.! HA.width "30%"

    summarizeCountStatistics bgBins fgBins

    liftIO $ let e = fmap proximityRatio fgBins
             in renderableToSVGFile
                (layoutFret fname (nbins p) e e [])
                640 480 (outputRoot++"-uncorrected.svg")

    let bgRate = mean . VU.fromList <$> unflipFrets bgBins
    (dOnlyBins, fretBins) <- case dOnlyFile p of
      Nothing  -> liftIO $ partitionDOnly (dOnlyCriterion p) fgBins
      Just dOnly -> do (dOnlyFg, dOnlyBg) <- liftIO
                          $  partition (\a->F.sum a > realToFrac (burstSize p))
                         <$> readFretBins fretChannels (realTimeToTime clk (binWidth p)) dOnly
                       let dOnlyBgRate = mean . VU.fromList <$> unflipFrets dOnlyBg
                           dOnlyFgCorrected = map (\a->(-) <$> a <*> dOnlyBgRate) dOnlyFg
                    
                       (_, fretBins) <- liftIO $ partitionDOnly (dOnlyCriterion p) fgBins
                       return (dOnlyFg, fretBins)
    analyzeBins p outputRoot fname bgRate dOnlyBins fretBins

fitFretHistogram :: FretAnalysis -> FilePath -> String
                 -> Int -> Gamma -> [Fret Double] -> HtmlLogT IO ()
fitFretHistogram p outputRoot title nComps gamma bins = do
    let fretEffs = map (fretEfficiency gamma) bins

    -- Fitting
    fitParams <- liftIO $ fitFret 200 nComps fretEffs
    tellLog 10 $ H.section $ do
        H.h2 "Fit"
        case fitParams of
          Nothing -> H.p "Fit failed"
          Just params ->
            H.ul $ forM_ (V.toList params) $ \(w,(a,b))->
              let (mu,sigma2) = paramToMoments (a,b)
                  s :: String
                  s = printf "weight=%1.3f, μ=%1.4f, σ²=%1.4f, mode=%1.4f, α=%1.3f, β=%1.3f" w mu sigma2 ((a-1)/(a+b-2)) a b
              in H.li $ H.toHtml s

    -- Plotting
    let shotSigma2 = shotNoiseEVarFromBins gamma bins
        fits = case fitParams of
                   Just ps ->
                       let mkBetas (w,p) =
                               let (mu,sigma2) = paramToMoments p
                               in [ (printf "fit <E>=%1.2f" mu, \e->w * realToFrac (betaProb p e))
                                  , ("shot-limited", \e->w * realToFrac (betaProb (paramFromMoments (mu,shotSigma2)) e))
                                  ]
                       in concatMap mkBetas $ VU.toList ps
                   Nothing -> []

    liftIO $ let layout = layoutFret title (nbins p) fretEffs fretEffs fits
             in renderableToSVGFile layout 640 480 (outputRoot++"-se.svg")

analyzeBins :: FretAnalysis -> FilePath -> String
            -> Fret Double -> [Fret Double] -> [Fret Double] -> HtmlLogT IO ()
analyzeBins p outputRoot title bgRate dOnlyBins fretBins = do
    tellLog 20
        $ let (mu,sig) = meanVariance $ VU.fromList $ fmap proximityRatio fretBins
          in H.section $ do
                 H.h2 "Uncorrected FRET"
                 H.ul $ do H.li $ H.toHtml $ meanHtml "E"++" = "++show mu
                           H.li $ H.toHtml $ varHtml "E"++" = "++show sig
                 H.img H.! HA.src (H.toValue $ outputRoot++"-uncorrected.svg")
                       H.! HA.width "30%"

    -- Corrections
    let bgBins = map (\bin->(-) <$> bin <*> bgRate) fretBins
        c = mean $ VU.fromList $ map crosstalkFactor dOnlyBins
        crosstalkAlpha = maybe c id $ crosstalk p -- TODO
        ctBins = fmap (correctCrosstalk crosstalkAlpha) bgBins
               :: [Fret Double]

    let g = gammaFromRates crosstalkAlpha
                           (fmap (mean . VU.fromList) $ unflipFrets dOnlyBins)
                           (fmap (mean . VU.fromList) $ unflipFrets fretBins)
        gamma' = maybe g id $ gamma p

    tellLog 5 $ H.section $ do
        H.h2 "Corrections"
        H.ul $ do
            H.li $ H.toHtml $ "Crosstalk = "++show crosstalkAlpha
            H.li $ H.toHtml $ "Estimated gamma (donor-only) = "++show g
            H.li $ H.toHtml $ "Effective gamma = "++show gamma'

    let fretEffs = map (fretEfficiency gamma') ctBins
    liftIO $ writeFile (outputRoot++"-fret-bins.txt") $ unlines
        $ zipWith3 (\e fret fretUncorr->intercalate "\t" $
                       [show e, "\t"]
                       ++map show (F.toList fret)++["\t"]
                       ++map show (F.toList fretUncorr)
                   ) fretEffs ctBins fretBins


    when (null fretBins) $ error "No FRET bins"

    fitFretHistogram p outputRoot title (fitComps p) gamma' ctBins
    let shotSigma2 = shotNoiseEVarFromBins gamma' ctBins
        (mu,sigma2) = meanVariance $ VU.fromList fretEffs
    tellLog 2 $ H.section $ do
        H.h2 "Corrected FRET efficiency"
        H.img H.! HA.src (H.toValue $ outputRoot++"-se.svg")
              H.! HA.width "50%" H.! HA.style "float: right;"
        H.ul $ do
            H.li $ H.toHtml $ meanHtml "E"++" = "++showFFloat (Just 4) mu ""
            H.li $ H.toHtml $ varHtml "E"++" = "++showFFloat (Just 4) sigma2 ""
            H.li $ H.toHtml $ "Shot-noise variance = "++showFFloat (Just 4) shotSigma2 ""


meanHtml x = "〈"++x++"〉"
varHtml x = meanHtml $ x++"² − "++meanHtml x++"²"

layoutThese :: (F.Foldable f, Applicative f, PlotValue x, PlotValue y, Num y)
            => (a -> Plot x y) -> f String -> f a -> Renderable ()
layoutThese f titles xs =
    renderLayout1sStacked $ F.toList
    $ pure makeLayout <*> titles <*> xs
    where --makeLayout :: String -> Plot x y -> Layout1 x y
          makeLayout title x = withAnyOrdinate
                               $ layout1_title .~ title
                               $ layout1_left_axis . laxis_override .~ (axis_viewport .~ vmap (0,150))
                               $ layout1_plots .~ [Left $ f x]
                               $ defaultLayout1

plotBinTimeseries :: [a] -> Plot Int a
plotBinTimeseries counts =
    toPlot
    $ plot_points_values .~ zip [0..] counts
    $ plot_points_style .~ filledCircles 0.5 (opaque blue)
    $ defaultPlotPoints

gaussianProb :: (Double,Double) -> Double -> Double
gaussianProb (mu,sigma2) x = exp (-(x-mu)^2 / 2 / sigma2) / sqrt (2*pi*sigma2)

layoutFret :: String -> Int -> [Double] -> [Double]
           -> [(String, FretEff -> Double)] -> Renderable ()
layoutFret title eBins e fretEs fits =
    let xs = [0.01,0.02..0.99]
        norm = realToFrac (length fretEs) / realToFrac eBins
        fit :: (String, FretEff -> Double) -> AlphaColour Double -> Plot Double Double
        fit (title,f) color =
              toPlot
              $ plot_lines_values .~ [map (\x->(x, f x * norm)) xs]
              $ plot_lines_title  .~ title
              $ plot_lines_style  .  line_color .~ color
              $ defaultPlotLines
        eHist = histToPlot
                $ plot_hist_bins .~ eBins
                $ plot_hist_values .~ V.fromList fretEs
                $ plot_hist_range .~ Just (0,1)
                $ defaultFloatPlotHist
        unitAxis = scaledAxis defaultLinearAxis (0,1)
    in toRenderable
       $ layout1_plots .~ ([Left eHist]++zipWith (\p color->Left $ fit p color)
                                               fits (colors $ length fits))
       $ layout1_bottom_axis . laxis_title    .~ "Proximity Ratio"
       $ layout1_bottom_axis . laxis_generate .~ unitAxis
       $ layout1_left_axis   . laxis_title    .~ "Occurrences"
       $ defaultLayout1

colors :: Int -> [AlphaColour Double]
colors n = map (\hue->opaque $ uncurryRGB sRGB $ hsv hue 0.8 0.8)
           [0,360 / realToFrac n..360]

layoutCountingHist :: (F.Foldable f, Applicative f, RealFrac counts, PlotValue counts)
                   => String -> Int -> f String -> f (AlphaColour Double) -> f (VB.Vector counts) -> Renderable ()
layoutCountingHist title maxBins names colours bins =
    let plot name color bins =
            histToPlot
            $ plot_hist_title .~ name
            $ plot_hist_values .~ bins
            $ plot_hist_fill_style .~ solidFillStyle color
            $ plot_hist_line_style .~ solidLine 1 color
            -- $ plot_hist_bins .~ maxBins
            $ defaultPlotHist                
    in toRenderable
       $ layout1_plots .~ map Left (F.toList $ plot <$> names <*> colours <*> bins)
       $ layout1_bottom_axis . laxis_title .~ "counts"
       $ layout1_left_axis   . laxis_title .~ "occurrences"
       $ defaultLayout1
