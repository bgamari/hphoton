{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens hiding (argument)
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
import           Pipes as P
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS
import           Pipes.Vector

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB

import           HPhoton.Bin
import           HPhoton.IO.FpgaTimetagger.Pipes
import           HPhoton.Fret
import           HPhoton.Types
import qualified Moments as M

import           Numeric
import           HtmlLog
import           FretFit
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Printf

import           Data.Default
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
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

import           Options.Applicative hiding ((&))

fileOpts :: FileOptions
fileOpts = FileOptions (500, 500) SVG

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
    <$> option auto
               ( long "clockrate" <> short 'c'
              <> value (round $ (128e6::Double))
              <> metavar "FREQ"
              <> help "Timetagger clockrate (Hz)"
               )
    <*> many (strArgument ( help "Input files" <> action "file" ))
    <*> option auto
               ( long "bin-width" <> short 'w'
              <> value 1e-3
              <> metavar "TIME"
              <> help "Width of temporal bins (seconds)"
               )
    <*> option auto
               ( long "burst-size" <> short 's'
              <> value 500
              <> metavar "N"
              <> help "Minimum burst rate in counts per bin"
               )
    <*> option auto
               ( long "upper-thresh" <> short 'u'
              <> value Nothing
              <> metavar "N"
              <> help "Throw out bins with greater than N photons"
               )
    <*> option auto
               ( long "nbins" <> short 'n'
              <> value 50
              <> metavar "N"
              <> help "Number of bins in the FRET efficiency histogram"
               )
    <*> option (Just <$> str)
               ( long "donly-file" <> short 'D'
              <> value Nothing
              <> help "Donor only file to use for gamma and crosstalk estimation; uses donor-only population of current file by default"
               )
    <*> option (let autoOption = eitherReader $ \s->if s == "auto" then pure Nothing else empty
                in autoOption <|> fmap Just auto)
               ( long "gamma" <> short 'g'
              <> value (Just 1)
              <> metavar "[N]"
              <> help "Gamma correct resulting histogram. If 'auto' is given, gamma will be estimated from the slope of the Donor-Acceptor population."
               )
    <*> option (let autoOption = eitherReader $ \s->if s == "auto" then pure Nothing else empty
                in autoOption <|> fmap Just auto)
               ( long "crosstalk" <> short 't'
              <> value (Just 0)
              <> metavar "[E]"
              <> help "Use crosstalk correction"
               )
    <*> strOption ( long "output" <> short 'o'
                 <> value "." <> metavar "DIR"
                 <> help "Directory in which to place output files"
                  )
    <*> option auto
               ( long "fit-comps" <> short 'f'
              <> value 1 <> metavar "N"
              <> help "Number of Beta fit components"
               )
    <*> dOnlyPartitioning

dOnlyPartitioning :: Parser DOnlyPartitioning
dOnlyPartitioning = eThresh <|> fitOdds
  where eThresh = EThresh
            <$> option auto
                       ( long "donly-thresh" <> short 'd'
                      <> value 0.2 <> metavar "E"
                      <> help "FRET efficiency cut-off for donor-only population"
                       )
        fitOdds = FitOdds
            <$> option auto
                       ( long "donly-fit-comps"
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

partitionDOnly :: DOnlyPartitioning -> VB.Vector (Fret Double) -> IO (VB.Vector (Fret Double), VB.Vector (Fret Double))
partitionDOnly (EThresh e) bins =
    return $ VB.partition (\b->proximityRatio b < e) bins
partitionDOnly (FitOdds nComps) bins = do
    fitParams <- maybe (error "Failed to fit for D-only paritioning") id
                 <$> fitFret 200 nComps (map proximityRatio $ VB.toList bins)
    let dOnlyOdds b = let e = proximityRatio b
                          probComp (w,c) e = realToFrac w * betaProb c e
                      in probComp (V.head fitParams) e / probComp (V.last fitParams) e
    return ( VB.filter (\b->dOnlyOdds b > 2) bins
           , VB.filter (\b->dOnlyOdds b < 1/2) bins
           )

readFretBins :: Fret Channel -> Time -> FilePath -> IO (VB.Vector (Fret Double))
readFretBins fretChannels binTime fname = do
    recs <- liftIO $ withFile fname ReadMode $ \fIn->
        runToVector $ runEffect
            $   PBS.fromHandle fIn
            >-> decodeRecords
            >-> PP.drop 1024
            >-> filterDeltas
            >-> toVector
    let times = unwrapTimes . strobeTimes recs <$> fretChannels :: Fret (VU.Vector Time)
        bins = fmap (fmap fromIntegral) $ binMany binTime times
    return bins

getFretBins :: FilePath -> Fret Channel -> Time -> FilePath -> HtmlLogT IO (VB.Vector (Fret Double))
getFretBins outputRoot fretChannels binTime fname = do
    bins <- liftIO $ readFretBins fretChannels binTime fname
    liftIO $ renderableToFile fileOpts (outputRoot++"-bins.svg")
        (layoutThese plotBinTimeseries (Fret "Acceptor" "Donor")
         $ unflipFrets $ take 10000 $ VB.toList bins)
    return bins

summarizeCountStatistics :: Monad m => VB.Vector (Fret Double) -> VB.Vector (Fret Double) -> HtmlLogT m ()
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
            rows [ ["Number of foreground bins", show $ VB.length fgBins] ]

            H.tr $ H.th "Background counts"
            H.tr $ mapM_ H.th ["", "mean", "variance"]
            fretRows $ fmap (\m->[ showFFloat (Just 2) (M.mean m) ""
                                 , showFFloat (Just 2) (M.variance m) ""
                                 ]) bgCountMoments
            rows [ ["Number of background bins", show $ VB.length bgBins] ]

goFile :: FretAnalysis -> FilePath -> IO ()
goFile p fname = writeHtmlLogT (fname++".html") $ do
    liftIO $ putStrLn fname
    tellLog 0 $ H.h1 $ H.toHtml fname
    tellLog 100 $ H.section $ do H.h2 "Analysis parameters"
                                 H.code $ H.toHtml $ show p
    let outputRoot = replaceDirectory fname (outputDir p)

    let fretChannels = Fret Ch1 Ch0
    let clk = clockFromFreq $ clockrate p
    (fgBins,bgBins) <- VB.partition (\a->F.sum a >= realToFrac (burstSize p))
                   <$> getFretBins outputRoot fretChannels (realTimeToTime clk (binWidth p)) fname
                    :: HtmlLogT IO (VB.Vector (Fret Double), VB.Vector (Fret Double))

    liftIO $ let names = Fret "acceptor" "donor"
                 colours = flip withOpacity 0.5 <$> Fret red green
                 layout = layoutCountingHist fname 100 names colours (fmap V.fromList $ unflipFrets $ V.toList fgBins)
             in renderableToFile fileOpts (outputRoot++"-pch.svg") layout
    tellLog 15 $ H.section $ do
        H.h2 "Photon Counting Histogram"
        H.img H.! HA.src (H.toValue $ fname++"-pch.svg")
              H.! HA.width "30%"

    summarizeCountStatistics bgBins fgBins

    liftIO $ let e = fmap proximityRatio fgBins
             in renderableToFile fileOpts (outputRoot++"-uncorrected.svg")
                (layoutFret fname (nbins p) (V.toList e) (V.toList e) [])

    let bgRate = mean . VU.fromList <$> unflipFrets (VB.toList bgBins)
    (dOnlyBins, fretBins) <- case dOnlyFile p of
      Nothing  -> liftIO $ partitionDOnly (dOnlyCriterion p) fgBins
      Just dOnly -> do (dOnlyFg, dOnlyBg) <- liftIO
                          $  VB.partition (\a->F.sum a > realToFrac (burstSize p))
                         <$> readFretBins fretChannels (realTimeToTime clk (binWidth p)) dOnly
                       let dOnlyBgRate = mean . VU.fromList <$> unflipFrets (V.toList dOnlyBg)
                           dOnlyFgCorrected = fmap (\a->(-) <$> a <*> dOnlyBgRate) dOnlyFg

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
            H.table $ do
              H.tr $ do
                H.th H.! HA.colspan "6" $ "fit"
                H.th H.! HA.colspan "6" $ "count statistics"
                H.th H.! HA.colspan "2" $ "FRET"
              H.tr $ mapM_ H.th [ "weight", "μ", "σ²", "mode", "α", "β"
                                , "Σ N_A", "Σ N_D"
                                , H.toHtml $ meanHtml "N_A", H.toHtml $ meanHtml "N_D"
                                , H.toHtml $ varHtml "N_A", H.toHtml $ varHtml "N_D"
                                , H.toHtml $ meanHtml "E", "N_A / N"
                                ]
              forM_ (zip [0..] $ V.toList params) $ \(i, (w,BetaParam a b))->H.tr $ do
                let (mu,sigma2) = paramToMoments (BetaParam a b)
                    mode = paramToMode (BetaParam a b)
                    assignments = filter (\bin->classify params (fretEfficiency gamma bin) == i) bins
                    Fret na nd = getSum <$> F.foldMap (fmap Sum) assignments
                mapM_ (H.td . H.toHtml) $
                  [ printf "%1.3f" w :: String
                  , printf "%1.4f" mu
                  , printf "%1.4f" sigma2
                  , printf "%1.4f" mode
                  , printf "%1.3f" a
                  , printf "%1.3f" b
                  , printf "%1.2e" na
                  , printf "%1.2e" nd
                  , printf "%1.3f" $ mean $ VU.fromList $ map fretA assignments
                  , printf "%1.3f" $ mean $ VU.fromList $ map fretD assignments
                  , printf "%1.3f" $ variance $ VU.fromList $ map fretA assignments
                  , printf "%1.3f" $ variance $ VU.fromList $ map fretD assignments
                  , printf "%1.3f" $ mean $ VU.fromList
                                   $ map (fretEfficiency gamma) assignments
                  , printf "%1.3f" $ na / (na+nd)
                  ]

    -- Plotting
    let shotSigma2 = shotNoiseEVarFromBins gamma bins
        fits = case fitParams of
                   Just ps ->
                       let mkBetas (w,p) =
                               let (mu,sigma2) = paramToMoments p
                                   mode = paramToMode p
                                   shotLimitedParams = paramFromMoments mu shotSigma2
                               in [ (printf "fit <E>=%1.2f" mu, \e->w * realToFrac (betaProb p e)) ]
                                 ++ F.foldMap (\p->[("shot-limited", \e->w * realToFrac (betaProb p e))]) shotLimitedParams
                       in concatMap mkBetas $ V.toList ps
                   Nothing -> []

    liftIO $ let layout = layoutFret title (nbins p) fretEffs fretEffs fits
             in renderableToFile fileOpts (outputRoot++"-se.svg") layout
    return ()

analyzeBins :: FretAnalysis -> FilePath -> String
            -> Fret Double -> VB.Vector (Fret Double) -> VB.Vector (Fret Double) -> HtmlLogT IO ()
analyzeBins p outputRoot title bgRate dOnlyBins fretBins = do
    tellLog 20
        $ let (mu,sig) = meanVariance $ fmap proximityRatio fretBins
          in H.section $ do
                 H.h2 "Uncorrected FRET"
                 H.ul $ do H.li $ H.toHtml $ meanHtml "E"++" = "++show mu
                           H.li $ H.toHtml $ varHtml "E"++" = "++show sig
                 H.img H.! HA.src (H.toValue $ outputRoot++"-uncorrected.svg")
                       H.! HA.width "30%"

    -- Corrections
    let bgBins = fmap (\bin->(-) <$> bin <*> bgRate) fretBins
        c = mean $ fmap crosstalkFactor dOnlyBins
        crosstalkAlpha = maybe c id $ crosstalk p -- TODO
        ctBins = fmap (correctCrosstalk crosstalkAlpha) bgBins
               :: VB.Vector (Fret Double)

    let g = gammaFromRates crosstalkAlpha
                           (fmap (mean . VU.fromList) $ unflipFrets $ V.toList dOnlyBins)
                           (fmap (mean . VU.fromList) $ unflipFrets $ V.toList fretBins)
        gamma' = maybe g id $ gamma p

    tellLog 5 $ H.section $ do
        H.h2 "Corrections"
        H.ul $ do
            H.li $ H.toHtml $ "Crosstalk = "++show crosstalkAlpha
            H.li $ H.toHtml $ "Estimated gamma (donor-only) = "++show g
            H.li $ H.toHtml $ "Effective gamma = "++show gamma'

    let fretEffs = fmap (fretEfficiency gamma') ctBins
    liftIO $ writeFile (outputRoot++"-fret-bins.txt") $ unlines $ VB.toList
        $ VB.zipWith3 (\e fret fretUncorr->intercalate "\t" $
                          [show e, "\t"]
                          ++map show (F.toList fret)++["\t"]
                          ++map show (F.toList fretUncorr)
                      ) fretEffs ctBins fretBins


    when (VB.null fretBins) $ error "No FRET bins"

    fitFretHistogram p outputRoot title (fitComps p) gamma' (VB.toList ctBins)
    let shotSigma2 = shotNoiseEVarFromBins gamma' (VB.toList ctBins)
        (mu,sigma2) = meanVariance fretEffs
    tellLog 2 $ H.section $ do
        H.h2 "Corrected FRET efficiency"
        H.img H.! HA.src (H.toValue $ outputRoot++"-se.svg")
              H.! HA.width "50%" H.! HA.style "float: right;"
        H.ul $ do
            H.li $ H.toHtml $ meanHtml "E"++" = "++showFFloat (Just 4) mu ""
            H.li $ H.toHtml $ varHtml "E"++" = "++showFFloat (Just 4) sigma2 ""
            H.li $ H.toHtml $ "Shot-noise variance = "++showFFloat (Just 4) shotSigma2 ""


meanHtml, varHtml :: String -> String
meanHtml x = "〈"++x++"〉"
varHtml x = meanHtml $ x++"² − "++meanHtml x++"²"

layoutThese :: (F.Foldable f, Applicative f, PlotValue x, PlotValue y, Num y)
            => (a -> Plot x y) -> f String -> f a -> Renderable ()
layoutThese f titles xs =
    renderStackedLayouts
    $ def
    & slayouts_layouts .~ (F.toList $ makeLayout <$> titles <*> xs)
    where --makeLayout :: String -> a -> StackedLayout y
          makeLayout title x =
              StackedLayout
              $ layout_title .~ title
              $ layout_y_axis . laxis_override .~ (axis_viewport .~ vmap (0,150))
              $ layout_plots .~ [f x]
              $ def

plotBinTimeseries :: [a] -> Plot Int a
plotBinTimeseries counts =
    toPlot
    $ plot_points_values .~ zip [0..] counts
    $ plot_points_style .~ filledCircles 0.5 (opaque blue)
    $ def

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
              $ def
        eHist = histToPlot
                $ plot_hist_bins .~ eBins
                $ plot_hist_values .~ V.fromList fretEs
                $ plot_hist_range .~ Just (0,1)
                $ defaultFloatPlotHist
        unitAxis = scaledAxis def (0,1) :: AxisFn Double
    in toRenderable
       $ layout_plots .~ ([eHist]++zipWith (\p color->fit p color)
                                               fits (colors $ length fits))
       $ layout_x_axis . laxis_title    .~ "Proximity Ratio"
       $ layout_x_axis . laxis_generate .~ unitAxis
       $ layout_y_axis . laxis_title    .~ "Occurrences"
       $ def

colors :: Int -> [AlphaColour Double]
colors n = map (\hue->opaque $ uncurryRGB sRGB $ hsv hue 0.8 0.8)
           [0,360 / realToFrac n..360]

layoutCountingHist :: (F.Foldable f, Applicative f, RealFrac counts, PlotValue counts)
                   => String -> Int -> f String -> f (AlphaColour Double)
                   -> f (VB.Vector counts) -> Renderable ()
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
       $ layout_plots .~ (F.toList $ plot <$> names <*> colours <*> bins)
       $ layout_x_axis . laxis_title .~ "counts"
       $ layout_y_axis . laxis_title .~ "occurrences"
       $ def
