{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import           Control.Lens hiding (each, argument)
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
import           Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS
import           Pipes.Vector

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB

import           HPhoton.Bin
import           HPhoton.IO.FpgaTimetagger.Alex
import           HPhoton.IO.FpgaTimetagger.Pipes
import           HPhoton.Fret (shotNoiseEVar)
import           HPhoton.Fret.Alex
import           HPhoton.Types
import           Numeric.MixtureModel.Beta
import qualified Moments as M

import           Numeric
import           HtmlLog
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

import           Numeric.SpecFunctions (logFactorial)
import           Numeric.Log hiding (sum)
import           Statistics.Types
import           Statistics.Sample
import           Statistics.Resampling as R
import           Statistics.Resampling.Bootstrap
import           Statistics.LinearRegression

import           Options.Applicative hiding ((&))

fileOpts :: FileOptions
fileOpts = FileOptions (500,500) SVG

type Rate = Double
type Gamma = Double

data AlexAnalysis = AlexAnalysis { clockrate :: Freq
                                 , input :: [FilePath]
                                 , binWidth :: Double
                                 , burstSize :: Int
                                 , fretThresh :: Int
                                 , nbins :: Int
                                 , initialTime :: Double
                                 , useCache :: Bool
                                 , gamma :: Maybe Double
                                 , crosstalk :: Maybe Crosstalk
                                 , dOnlyThresh :: Double
                                 , aOnlyThresh :: Double
                                 , outputDir :: FilePath
                                 }
                    deriving (Show, Eq)

alexAnalysis :: Parser AlexAnalysis
alexAnalysis = AlexAnalysis
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
              <> help "Width of temporal bins"
               )
    <*> option auto
               ( long "burst-size" <> short 's'
              <> value 500
              <> metavar "N"
              <> help "Minimum burst rate in Hz"
               )
    <*> option auto
               ( long "fret-thresh" <> short 'f'
              <> value 10
              <> metavar "N"
              <> help "Minimum number of photons in Dexc channels to include bin in FRET histogram"
               )
    <*> option auto
               ( long "nbins" <> short 'n'
              <> value 50
              <> metavar "N"
              <> help "Number of bins in the FRET efficiency histogram"
               )
    <*> option auto
               ( long "initial-time" <> short 'i'
              <> value 10e-6
              <> metavar "TIME"
              <> help "Initial time of bin to drop"
               )
    <*> switch ( long "use-cache" <> short 'C'
              <> help "Use trimmed delta cache"
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
              <> metavar "[N]"
              <> help "Use crosstalk correction"
               )
    <*> option auto
               ( long "d-only-thresh" <> short 'D'
              <> value 0.85 <> metavar "S"
              <> help "Stoiciometry threshold for identification of donor-only population"
               )
    <*> option auto
               ( long "a-only-thresh" <> short 'A'
              <> value 0.20 <> metavar "S"
              <> help "Stoiciometry threshold for identification of acceptor-only population"
               )
    <*> strOption ( long "output" <> short 'o'
              <> value "." <> metavar "DIR"
              <> help "Directory in which to place output files"
               )

poissonP :: Rate -> Int -> Log Double
poissonP l k = l'^k / factorial' k * realToFrac (exp (-l))
    where l' = realToFrac l
          factorial' = Exp . logFactorial

bgOdds :: Rate -> Rate -> Int -> Log Double
bgOdds bg fg k = poissonP fg k / poissonP bg k

main = do
    let opts = info (helper <*> alexAnalysis)
                    ( fullDesc
                   <> progDesc "ALEX FRET analysis"
                    )
    p <- execParser opts
    createDirectoryIfMissing True (outputDir p)
    forM_ (input p) $ goFile p

filterBinsBayes :: RealTime -> Alex Rate -> Alex Rate -> Alex Int -> Bool
filterBinsBayes binWidth bgRate fgRate alex =
    F.product ( pure bgOdds
               <*> fmap (* binWidth) bgRate
               <*> fmap (* binWidth) fgRate
               <*> alex
               ) > 2

-- | Strict foldMap
foldMap' :: (F.Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap' f = F.foldl' (\m a->mappend m $! f a) mempty

readAlexData :: Bool -> FilePath -> IO (VU.Vector Record)
readAlexData useCache fname = do
    let trimFName = "."++fname++".trimmed"
    cacheExists <- liftIO $ doesFileExist trimFName
    let fname' = if cacheExists && useCache then trimFName else fname
    recs <- liftIO $ withFile fname' ReadMode $ \fIn->
        runToVector $ runEffect
            $   PBS.fromHandle fIn
            >-> decodeRecords
            >-> PP.drop 1024
            >-> filterDeltas
            >-> toVector

    when (useCache && not cacheExists)
        $ liftIO $ withFile trimFName WriteMode $ \fOut->
        runEffect $ each (V.toList recs) >-> encodeRecords >-> PBS.toHandle fOut

    return recs

goFile :: AlexAnalysis -> FilePath -> IO ()
goFile p fname = writeHtmlLogT (fname++".html") $ do
    liftIO $ putStrLn fname
    tellLog 0 $ H.h1 $ H.toHtml fname
    tellLog 100 $ H.section $ do H.h2 "Analysis parameters"
                                 H.code $ H.toHtml $ show p

    let outputRoot = replaceDirectory fname (outputDir p)
    recs <- liftIO $ readAlexData (useCache p) fname

    let alexChannels = AlexChannels { alexExc = Fret Ch1 Ch0
                                    , alexEm  = Fret Ch1 Ch0
                                    }
    let clk = clockFromFreq $ round (128e6::Double)
    let times = alexTimes (realTimeToTime clk (initialTime p)) alexChannels recs
        a = fromIntegral (burstSize p) * binWidth p
        thresh = Alex { alexAexcAem = 200, alexAexcDem = 0
                      , alexDexcAem = 200, alexDexcDem = 200 }
        bgRates = Alex { alexAexcAem = 50, alexAexcDem = 50
                       , alexDexcAem = 50, alexDexcDem = 50 }
        fgRates = Alex { alexAexcAem = 10000, alexAexcDem = 50
                       , alexDexcAem = 10000, alexDexcDem = 10000 }
        (bins,bgBins) =
            --  VB.filter (\alex->getAll $ F.fold
            --               $ pure (\a b->All $ a >= b) <*> alex <*> fmap (*binWidth p) thresh)
              VB.partition (\a->alexAexcAem a+alexDexcAem a+alexDexcDem a > realToFrac (burstSize p))
            $ fmap (fmap fromIntegral)
            -- $ VB.filter (filterBinsBayes (binWidth p) bgRates fgRates)
            $ binMany (realTimeToTime clk (binWidth p)) times
            :: (VB.Vector (Alex Double), VB.Vector (Alex Double))

    let fgCountMoments = foldMap' (fmap M.sample) bins
        bgCountMoments = foldMap' (fmap M.sample) bgBins
    tellLog 10 $ H.section $ do
        H.h2 "Count statistics"
        let rows :: H.ToMarkup a => [[a]] -> H.Html
            rows = mapM_ (H.tr . mapM_ (H.td . H.toHtml))
            alexRows :: Alex [String] -> H.Html
            alexRows alex = rows [ ["A excitation, A emission"] ++ alexAexcAem alex
                                 , ["A excitation, D emission"] ++ alexAexcDem alex
                                 , ["D excitation, A emission"] ++ alexDexcAem alex
                                 , ["D excitation, D emission"] ++ alexDexcDem alex
                                 ]
        H.table $ do
            H.tr $ H.th "Total counts"
            alexRows $ fmap (\x->[showFFloat (Just 1) x ""]) $ fmap getSum
                     $ foldMap' (fmap Sum) bins <> foldMap' (fmap Sum) bgBins
            H.tr $ H.th "Foreground counts"
            H.tr $ mapM_ H.th ["", "mean", "variance"]
            alexRows $ fmap (\m->[ showFFloat (Just 2) (M.mean m) ""
                                 , showFFloat (Just 2) (M.variance m) ""
                                 ]) fgCountMoments
            rows [ ["Number of foreground bins", show $ VB.length bins] ]
            H.tr $ H.th "Background counts"
            H.tr $ mapM_ H.th ["", "mean", "variance"]
            alexRows $ fmap (\m->[ showFFloat (Just 2) (M.mean m) ""
                                 , showFFloat (Just 2) (M.variance m) ""
                                 ]) bgCountMoments
            rows [ ["Number of background bins", show $ VB.length bgBins] ]

    liftIO $ renderableToFile fileOpts (fname++"-uncorrected.svg")
        (layoutSE fname (nbins p) (VB.toList $ fmap stoiciometry bins)
                                  (VB.toList $ fmap proxRatio bins)
                                  (VB.toList $ fmap proxRatio bins)
                                  [])
    when (V.null bins) $ fail "No bins"
    tellLog 20
        $ let ((muS,sigS), (muE,sigE)) =
                           (\(s,e)->(meanVariance s, meanVariance e))
                         $ VB.unzip
                         $ VB.filter (\(s,e)->s < dOnlyThresh p)
                         $ VB.zip (fmap stoiciometry bins) (fmap proxRatio bins)
          in H.section $ do
                 H.h2 "Uncorrected FRET"
                 H.ul $ do H.li $ H.toHtml $ meanHtml "E"++" = "++show muE
                           H.li $ H.toHtml $ varHtml  "E"++" = "++show sigE
                           H.li $ H.toHtml $ meanHtml "S"++" = "++show muS
                           H.li $ H.toHtml $ varHtml  "S"++" = "++show sigS
                 H.img H.! HA.src (H.toValue $ fname++"-uncorrected.svg")
                       H.! HA.width "30%" H.! HA.style "float: right;"

    let aOnlyBins = VB.filter (\alex->stoiciometry alex < aOnlyThresh p)
                    $ VB.filter (\alex->alexDexcDem alex + alexDexcAem alex > realToFrac (fretThresh p))
                    $ bins
        (dirD,dirDVar) = meanVariance $ fmap directAExc aOnlyBins

    let bgRate = fmap M.mean bgCountMoments
        bgBins = fmap (\bin->(-) <$> bin <*> bgRate) bins
        (dOnlyBins, fretBins) = VB.partition (\alex->stoiciometry alex > dOnlyThresh p) bgBins
        a = mean $ fmap crosstalkFactor dOnlyBins
        crosstalkAlpha = maybe a id $ crosstalk p
        ctBins = fmap (correctDirectAExc dirD . correctCrosstalk crosstalkAlpha) bgBins

    let (beta,g) = estimateGamma $ V.convert
            $ VB.filter (\(s,e) -> s < dOnlyThresh p)
            $ VB.zip (fmap stoiciometry ctBins) (fmap proxRatio ctBins)
        dSd = mean (fmap alexDexcDem fretBins) - mean (fmap alexDexcDem dOnlyBins)
        dSa = mean (fmap alexDexcAem fretBins) - mean (fmap alexDexcAem dOnlyBins)
        g2 = crosstalkAlpha - dSa / dSd
        gamma' = maybe (g) id $ gamma p

    tellLog 5 $ H.section $ do
        H.h2 "Corrections"
        H.ul $ do
            H.li $ H.toHtml $ "Direct acceptor excitation = "++show dirD++" +- "++show dirDVar
            H.li $ H.toHtml $ "Crosstalk = "++show crosstalkAlpha
            H.li $ H.toHtml $ "Estimated beta (slope) = "++show beta
            H.li $ H.toHtml $ "Estimated gamma (slope) = "++show g
            H.li $ H.toHtml $ "Estimated gamma (donor-only) = "++show g2
            H.li $ H.toHtml $ "Effective gamma = "++show gamma'

    let s = fmap (stoiciometry' gamma') ctBins
        e = fmap (fretEff gamma') ctBins
    liftIO $ writeFile (outputRoot++"-se") $ unlines $ VB.toList
        $ VB.zipWith4 (\s e alex alexUncorr->intercalate "\t" $
                          [show s, show e, "\t"]
                          ++map show (F.toList alex)++["\t"]
                          ++map show (F.toList alexUncorr)
                      ) s e ctBins bins

    let fretBins = VB.filter (\a->let s = stoiciometry' gamma' a
                                  in s < dOnlyThresh p && s > aOnlyThresh p
                             ) ctBins
    let (mu,sigma2) = meanVariance $ fmap (fretEff gamma') fretBins
        nInv = mean
               $ fmap (\alex->1 / realToFrac (alexDexcAem alex + alexDexcDem alex))
               $ fretBins
        shotSigma2 = shotNoiseEVar (1/nInv) mu

    liftIO $ renderableToFile fileOpts (outputRoot++"-se.svg") $
        layoutSE fname (nbins p) (VB.toList s)
                                 (VB.toList e)
                                 (VB.toList $ fmap (fretEff gamma') fretBins)
                  $  F.foldMap (\p->[("shot-limited", Beta p)]) (paramFromMoments mu shotSigma2)
                  ++ F.foldMap (\p->[(printf "fit 〈E〉=%1.2f" mu, Beta p)]) (paramFromMoments mu sigma2)
                  --  ("fit", Gaussian (mu, sigma2))
                  --, ("shot-limited", Gaussian (mu, shotSigma2))

    tellLog 2 $ H.section $ do
        H.h2 "Corrected FRET efficiency"
        H.img H.! HA.src (H.toValue $ outputRoot++"-se.svg")
              H.! HA.width "50%" H.! HA.style "float: right;"
        H.ul $ do
            H.li $ H.toHtml $ meanHtml "E"++" = "++showFFloat (Just 4) mu ""
            H.li $ H.toHtml $ varHtml "E"++" = "++showFFloat (Just 4) sigma2 ""
            H.li $ H.toHtml $ "Shot-noise variance = "++showFFloat (Just 4) shotSigma2 ""
            H.li $ H.toHtml $
              let e = fmap (fretEff gamma') fretBins
                  bootstrap = bootstrapBCA cl90 (V.convert e) [(VarianceUnbiased, Bootstrap (R.estimate VarianceUnbiased (V.convert e)) resamp)]
                  resamp = jackknife VarianceUnbiased (V.convert e)
              in "Bootstrap variance = "++show bootstrap

    liftIO $ renderableToFile fileOpts (outputRoot++"-bins.svg")
        $ layoutThese plotBinTimeseries (Alex "AA" "AD" "DD" "DA") $ T.sequenceA $ VB.toList bins
    return ()

meanHtml x = "〈"++x++"〉"
varHtml x = meanHtml $ x++"² − "++meanHtml x++"²"

-- | Estimate gamma from slope in E-S plane
estimateGamma :: VU.Vector (Double, Double) -> (Double, Double)
estimateGamma xs =
    let (omega,sigma) = linearRegression (V.map (\(e,s)->1/s) xs) (V.map (\(e,s)->e) xs)
        beta = omega + sigma - 1
        gamma = (omega - 1) / (omega + sigma - 1)
    in (beta, gamma)

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

data Fit = Gaussian (Double, Double) -- ^ (Mean, Variance)
         | Beta BetaParam
         deriving (Show)

gaussianProb :: (Double,Double) -> Double -> Double
gaussianProb (mu,sigma2) x = exp (-(x-mu)^2 / 2 / sigma2) / sqrt (2*pi*sigma2)

layoutSE :: String -> Int -> [Double] -> [Double] -> [Double]
         -> [(String, Fit)] -> Renderable ()
layoutSE title eBins s e fretEs fits =
    let pts = toPlot
              $ plot_points_values .~ zip e s
              $ plot_points_style .~ filledCircles 2 (opaque blue)
              $ def
        xs = [0.01,0.02..0.99]
        norm = realToFrac (length fretEs) / realToFrac eBins
        fit :: (String, Fit) -> AlphaColour Double -> Plot Double Double
        fit (title,param) color =
              let f = case param of Gaussian p -> gaussianProb p
                                    Beta p     -> realToFrac . betaProb p
              in toPlot
                 $ plot_lines_values .~ [map (\x->(x, f x * norm)) xs]
                 $ plot_lines_title  .~ title
                 $ plot_lines_style  .  line_color .~ color
                 $ def
        eHist = histToPlot
                $ plot_hist_bins .~ eBins
                $ plot_hist_values .~ fretEs
                $ plot_hist_range .~ Just (0,1)
                $ defaultFloatPlotHist
        unitAxis = scaledAxis def (0,1) :: AxisFn Double
        layouts =
          [ StackedLayout
            $ layout_title .~ title
            $ layout_plots .~ [pts]
            $ layout_x_axis . laxis_generate .~ unitAxis
            $ layout_y_axis . laxis_generate .~ unitAxis
            $ layout_y_axis . laxis_title .~ "Stoiciometry"
            $ def
          , StackedLayout
            $ layout_plots .~ ([eHist]++zipWith (\p color->fit p color)
                                                     fits (colors $ length fits))
            $ layout_x_axis . laxis_title .~ "Proximity Ratio"
            $ layout_x_axis . laxis_generate .~ unitAxis
            $ layout_y_axis . laxis_title .~ "Occurrences"
            $ def
          ]
    in renderStackedLayouts $ def & slayouts_layouts .~ layouts

colors :: Int -> [AlphaColour Double]
colors n = map (\hue->opaque $ uncurryRGB sRGB $ hsv hue 0.8 0.8)
           [0,360 / realToFrac n..360]
