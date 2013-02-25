import           Control.Lens hiding ((^=), (.>))
import           Data.Accessor
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.List (partition)
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive

import           System.IO
import           System.Directory (doesFileExist)
import           System.FilePath
import           Control.Proxy as P
import qualified Control.Proxy.ByteString as PBS
import           Control.Proxy.Vector

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU

import           HPhoton.Types
import           HPhoton.FpgaTimetagger.Pipe
import           HPhoton.FpgaTimetagger.Alex
import           HPhoton.Bin.Alex
import           HPhoton.Fret.Alex
import           HPhoton.Fret (shotNoiseEVar)

import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Plot.Histogram
import           Data.Colour
import           Data.Colour.Names

import           Numeric.SpecFunctions (logFactorial)
import           Data.Number.LogFloat hiding (realToFrac, isNaN)
import           Statistics.Sample
import           Statistics.LinearRegression
                 
import           Options.Applicative

type Rate = Double

data AlexAnalysis = AlexAnalysis { clockrate :: Freq
                                 , input :: [FilePath]
                                 , binWidth :: Double
                                 , burstSize :: Int
                                 , nbins :: Int
                                 , initialTime :: Double
                                 , useCache :: Bool
                                 , gamma :: Maybe Double
                                 , crosstalk :: Maybe Double
                                 , dOnlyThresh :: Double
                                 , outputDir :: FilePath
                                 }
                    deriving (Show, Eq)

alexAnalysis :: Parser AlexAnalysis
alexAnalysis = AlexAnalysis
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
              <> help "Minimum burst rate in Hz"
               )
    <*> option ( long "nbins" <> short 'n'
              <> value 50
              <> metavar "N"
              <> help "Number of bins in the FRET efficiency histogram"
               )
    <*> option ( long "initial-time" <> short 'i'
              <> value 10e-6
              <> metavar "TIME"
              <> help "Initial time of bin to drop"
               )
    <*> switch ( long "use-cache" <> short 'C'
              <> help "Use trimmed delta cache"
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
    <*> option ( long "d-only-thresh" <> short 'D'
              <> value 0.85 <> metavar "S"
              <> help "Stoiciometry threshold for identification of donor-only population"
               )
    <*> strOption ( long "output" <> short 'o'
              <> value "." <> metavar "DIR"
              <> help "Directory in which to place output files"
               )

poissonP :: Rate -> Int -> LogFloat
poissonP l k = l'^k / factorial' k * realToFrac (exp (-l))
    where l' = realToFrac l
          factorial' = logToLogFloat . logFactorial

bgOdds :: Rate -> Rate -> Int -> LogFloat
bgOdds bg fg k = poissonP fg k / poissonP bg k

data Average a = Average !Int !a deriving (Read, Show)
    
instance Num a => Monoid (Average a) where
    mempty = Average 0 0
    Average n a `mappend` Average m b = Average (n+m) (a+b)
    
runAverage :: Fractional a => Average a -> a
runAverage (Average n a) = a / fromIntegral n

main = do
    let opts = info (helper <*> alexAnalysis)
                    ( fullDesc
                   <> progDesc "ALEX FRET analysis"
                    )
    p <- execParser opts
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

goFile :: AlexAnalysis -> FilePath -> IO ()
goFile p fname = do
    let trimFName = "."++fname++".trimmed"
    let outputRoot = replaceDirectory fname (outputDir p)
    cacheExists <- doesFileExist trimFName
    let fname' = if cacheExists && useCache p then trimFName else fname
    recs <- withFile fname' ReadMode $ \fIn->
        runToVectorD $ runProxy $   raiseK (PBS.fromHandleS fIn)
                                >-> decodeRecordsP
                                >-> dropD 1024
                                >-> filterDeltasP
                                >-> toVectorD
    
    when (useCache p && not cacheExists) $ withFile trimFName WriteMode $ \fOut->
        runProxy $ fromListS (V.toList recs) >-> encodeRecordsP >-> PBS.toHandleD fOut

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
            --  filter (\alex->getAll $ F.fold
            --               $ pure (\a b->All $ a >= b) <*> alex <*> fmap (*binWidth p) thresh)
              partition (\alex->F.sum alex > realToFrac (burstSize p))
            $ fmap (fmap fromIntegral)
            -- $ filter (filterBinsBayes (binWidth p) bgRates fgRates)
            $ alexBin (realTimeToTime clk (binWidth p)) times
            :: ([Alex Double], [Alex Double])

    putStrLn $ "\n    "++fname
    putStrLn $ "Bin count = "++show (length bins)
    let counts = runAverage <$> foldMap' (fmap (Average 1)) bins
        bgCounts = runAverage <$> foldMap' (fmap (Average 1)) bgBins
    putStrLn $ "counts = "++show counts
    putStrLn $ "background counts = "++show bgCounts

    renderableToPDFFile
        (layoutSE (nbins p) (fmap stoiciometry bins) (fmap proxRatio bins))
        640 480 (fname++"-uncorrected.pdf")
    putStrLn $ let (mu,sig) = meanVariance $ VU.fromList
                              $ map snd
                              $ filter (\(s,e)->s < dOnlyThresh p)
                              $ zip (fmap stoiciometry bins) (fmap proxRatio bins)
               in "uncorrected <E>="++show mu++"  <(E - <E>)^2>="++show sig
             
    let aOnlyThresh = 0.2
        d = map directAExc
            $ filter (\alex->stoiciometry alex < aOnlyThresh)
            $ bins
        (dirD, dirDVar) = meanVariance $ VU.fromList d
    putStrLn $ "Dir = "++show (dirD, dirDVar)


    let bgBins = map (\bin->(-) <$> bin <*> bgCounts) bins
        (dOnlyBins, fretBins) = partition (\alex->stoiciometry alex > dOnlyThresh p) bgBins
        a = mean $ VU.fromList
            $ map (\alex->alexDexcAem alex / alexDexcDem alex)
            $ dOnlyBins
        crosstalkAlpha = maybe a id $ crosstalk p
        ctBins = fmap (\alex->let lk = crosstalkAlpha * alexDexcDem alex
                                  dir = dirD * alexAexcAem alex
                              in alex { alexDexcAem = alexDexcAem alex - lk - dir }
                      ) bgBins
    putStrLn $ "Crosstalk = "++show crosstalkAlpha 
    
    let (beta,g) = estimateGamma $ V.fromList
            $ filter (\(s,e) -> s < dOnlyThresh p)
            $ zip (fmap stoiciometry ctBins) (fmap proxRatio ctBins)
        dSd = mean (VU.fromList $ map alexDexcDem fretBins) - mean (VU.fromList $ map alexDexcDem dOnlyBins)
        dSa = mean (VU.fromList $ map alexDexcAem fretBins) - mean (VU.fromList $ map alexDexcAem dOnlyBins)
        g2 = crosstalkAlpha - dSa / dSd
        gamma' = maybe (g) id $ gamma p
    putStrLn $ "Estimated gamma = "++show g
    putStrLn $ "Estimated gamma = "++show g2
    putStrLn $ "gamma = "++show gamma'

    let s = fmap (stoiciometry' gamma') ctBins
        e = fmap (fretEff gamma') ctBins
    writeFile (outputRoot++"-se") $ unlines
        $ zipWith3 (\s e alex->show s++"\t"++show e++F.foldMap (\a->"\t"++show a) alex) s e bins

    putStrLn $ let (mu,sig) = meanVariance $ VU.fromList
                              $ map snd
                              $ filter (\(s,e)->s < dOnlyThresh p)
                              $ zip s e
                   nInv = mean $ VU.fromList
                          $ filter (/= 0)
                          $ map (\alex->alexDexcAem alex + alexDexcDem alex)
                          $ filter (\alex->stoiciometry alex > aOnlyThresh)
                          $ filter (\alex->stoiciometry alex < dOnlyThresh p)
                          $ ctBins
                   shotSig = shotNoiseEVar nInv mu
               in "<E>="++show mu++"  <(E - <E>)^2>="++show sig++"  <1/N>="++show nInv++"  shot-noise variance="++show shotSig

    renderableToPDFFile (layoutSE (nbins p) s e) 640 480 (outputRoot++"-se.pdf")
    
    renderableToPDFFile 
        (layoutThese plotBinTimeseries (Alex "AA" "AD" "DD" "DA") $ T.sequenceA bins)
        500 500 (outputRoot++"-bins.pdf")

-- | Estimate gamma from slope in E-S plane
estimateGamma :: VU.Vector (Double, Double) -> (Double, Double)
estimateGamma xs =
    let (omega,sigma) = linearRegression (V.map (\(e,s)->1/s) xs) (V.map (\(e,s)->e) xs)
        beta = omega + sigma - 1
        gamma = (omega - 1) / (omega + sigma - 1)
    in (beta, gamma)

-- | Estimate gamma from donor-only population
estimateGammaDonor :: VU.Vector (Alex Rate) -> VU.Vector (Alex Rate) -> Double
estimateGammaDonor donorOnly donorAcceptor = undefined
    

layoutThese :: (F.Foldable f, Applicative f, PlotValue x, PlotValue y, Num y)
            => (a -> Plot x y) -> f String -> f a -> Renderable ()
layoutThese f titles xs =
    renderLayout1sStacked $ F.toList
    $ pure makeLayout <*> titles <*> xs
    where --makeLayout :: String -> Plot x y -> Layout1 x y
          makeLayout title x = withAnyOrdinate
                               $ layout1_title ^= title
                               $ layout1_plots ^= [Left $ f x]
                               $ defaultLayout1

plotBinTimeseries :: [a] -> Plot Int a
plotBinTimeseries counts =
    toPlot
    $ plot_points_values ^= zip [0..] counts
    $ plot_points_style ^= filledCircles 0.5 (opaque blue)
    $ defaultPlotPoints
    
layoutSE :: Int -> [Double] -> [Double] -> Renderable ()
layoutSE eBins s e =
    let pts = toPlot
              $ plot_points_values ^= zip e s
              $ plot_points_style ^= filledCircles 2 (opaque blue)
              $ defaultPlotPoints
        eHist = histToPlot
                $ plot_hist_bins ^= eBins
                $ plot_hist_values ^= e
                $ plot_hist_range ^= Just (0,1)
                $ defaultPlotHist
    in renderLayout1sStacked
       [ withAnyOrdinate
         $ layout1_plots ^= [Left pts]
         $ layout1_bottom_axis .> laxis_override ^= (axis_viewport ^= vmap (0,1))
         $ layout1_left_axis   .> laxis_override ^= (axis_viewport ^= vmap (0,1))
         $ layout1_left_axis   .> laxis_title ^= "Stoiciometry"
         $ defaultLayout1
       , withAnyOrdinate
         $ layout1_plots ^= [Left eHist]
         $ layout1_bottom_axis .> laxis_title ^= "Proximity Ratio"
         $ layout1_bottom_axis .> laxis_override ^= (axis_viewport ^= vmap (0,1))
         $ layout1_left_axis   .> laxis_title ^= "Occurrences"
         $ defaultLayout1
       ]
