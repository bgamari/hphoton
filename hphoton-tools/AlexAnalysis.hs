import           Control.Lens hiding ((^=), (.>))
import           Data.Accessor
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive

import           System.IO
import           System.Directory (doesFileExist)
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

import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Plot.Histogram
import           Data.Colour
import           Data.Colour.Names

import           Numeric.SpecFunctions (logFactorial)
import           Data.Number.LogFloat hiding (realToFrac)
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
                                 , gamma :: Double
                                 , crosstalk :: Bool
                                 , dOnlyThresh :: Double
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
    <*> option ( long "gamma" <> short 'g'
              <> value 1
              <> metavar "N"
              <> help "Plot assuming given gamma"
               )
    <*> switch ( long "crosstalk" <> short 't'
              <> help "Use crosstalk correction"
               )
    <*> option ( long "d-only-thresh" <> short 'D'
              <> value 0.85 <> metavar "S"
              <> help "Stoiciometry threshold for identification of donor-only population"
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

goFile :: AlexAnalysis -> FilePath -> IO ()
goFile p fname = do
    let trimFName = "."++fname++".trimmed"
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
        thresh = Alex { alexAexcAem = a, alexAexcDem = 0
                      , alexDexcAem = a, alexDexcDem = a }
        bgRates = Alex { alexAexcAem = 40, alexAexcDem = 50
                       , alexDexcAem = 40, alexDexcDem = 40 }
        fgRates = Alex { alexAexcAem = 5000, alexAexcDem = 50
                       , alexDexcAem = 5000, alexDexcDem = 5000 }
        bins = --  filter (\alex->getAll $ F.fold
               --                 $ pure (\a b->All $ a >= b) <*> alex <*> thresh)
               -- $ filter (\alex->getSum (F.foldMap Sum alex) > burstSize p)
                 fmap (fmap fromIntegral)
               $ filter (\alex->F.product ( pure bgOdds
                                        <*> fmap (* binWidth p) bgRates
                                        <*> fmap (* binWidth p) fgRates
                                        <*> alex
                                          ) > 2)
               $ alexBin (realTimeToTime clk (binWidth p)) times
             :: [Alex Double]

    let counts = pure (runAverage . mconcat) <*> T.sequenceA (map (pure (Average 1) <*>) bins)
    putStrLn $ "Counts = "++show counts
             
    let crosstalkAlpha = if crosstalk p
                              then mean $ VU.fromList
                                   $ map snd
                                   $ filter (\(s,e) -> s > dOnlyThresh p)
                                   $ zip (fmap stoiciometry bins) (fmap proxRatio bins)
                              else 1
    putStrLn $ "Crosstalk = "++show crosstalkAlpha 
          
    let g = estimateGamma $ V.fromList
            $ filter (\(s,e) -> s < dOnlyThresh p)
            $ zip (fmap stoiciometry bins) (fmap proxRatio bins)
    putStrLn $ "Gamma = "++show g

    let s = fmap (stoiciometry' (gamma p)) bins
        e = fmap (fretEff (gamma p)) bins
    writeFile (fname++"-se") $ unlines
        $ zipWith3 (\s e alex->show s++"\t"++show e++F.foldMap (\a->"\t"++show a) alex) s e bins

    renderableToPDFFile (layoutSE (nbins p) s e) 640 480 (fname++"-se.pdf")
    
    renderableToPDFFile 
        (layoutThese plotBinTimeseries (Alex "AA" "AD" "DD" "DA") $ T.sequenceA bins)
        500 500 (fname++"-bins.pdf")

estimateGamma :: VU.Vector (Double, Double) -> (Double, Double)
estimateGamma xs =
    let (omega,sigma) = linearRegression (V.map (\(e,s)->1/s) xs) (V.map (\(e,s)->e) xs)
        beta = omega + sigma - 1
        gamma = (omega - 1) / (omega + sigma - 1)
    in (beta, gamma)

layoutThese :: (F.Foldable f, Applicative f, PlotValue x, PlotValue y)
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
        e_hist = histToPlot
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
         $ layout1_plots ^= [Left e_hist]
         $ layout1_bottom_axis .> laxis_title ^= "Proximity Ratio"
         $ layout1_bottom_axis .> laxis_override ^= (axis_viewport ^= vmap (0,1))
         $ layout1_left_axis   .> laxis_title ^= "Occurrences"
         $ defaultLayout1
       ]
