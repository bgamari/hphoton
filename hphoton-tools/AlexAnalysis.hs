import           Control.Lens hiding ((^=), (.>))
import           Data.Accessor
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           System.IO

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

import           Numeric.SpecFunctions (factorial)
                 
import           Options.Applicative

type Rate = Double

data AlexAnalysis = AlexAnalysis { clockrate :: Freq
                                 , input :: [FilePath]
                                 , burst_size :: Int
                                 , nbins :: Int
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
    <*> option ( long "burst-size" <> short 's'
              <> value 2
              <> metavar "N"
              <> help "Minimum burst size in photons"
               )
    <*> option ( long "nbins" <> short 'n'
              <> value 50
              <> metavar "N"
              <> help "Number of bins in the FRET efficiency histogram"
               )

poissonP :: Rate -> Int -> Double
poissonP l k = l^k / factorial k * exp (-l)

bgOdds :: Rate -> Rate -> Int -> Double
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
    recs <- withFile fname ReadMode $ \fIn->
        runToVectorD $ runProxy $   raiseK (PBS.fromHandleS fIn)
                                >-> decodeRecordsP
                                >-> dropD 1024
                                >-> filterDeltasP
                                -- >-> takeB 100000
                                >-> toVectorD
    
    let alexChannels = AlexChannels { alexExc = Fret Ch1 Ch0
                                    , alexEm  = Fret Ch1 Ch0
                                    }
    let clk = clockFromFreq $ round (128e6::Double)
    let times = alexTimes 0 alexChannels recs
        thresh = Alex { alexAexcAem = 0           , alexAexcDem = 0
                      , alexDexcAem = burst_size p, alexDexcDem = burst_size p }
        bins = fmap (fmap fromIntegral)
               $ filter (\alex->getAll $ F.fold
                                $ pure (\a b->All $ a > b) <*> alex <*> thresh)
               -- $ filter (\alex->getSum (F.foldMap Sum alex) > burst_size p)
               $ alexBin (realTimeToTime clk 10e-3) times
             :: [Alex Double]

    let counts = pure (runAverage . mconcat) <*> T.sequenceA (map (pure (Average 1) <*>) bins)
    putStrLn $ "Counts = "++show counts

    writeFile (fname++"-bins") $ unlines $ map (F.foldMap (\a->show a++"\t")) bins
    let s = fmap stoiciometry bins
        e = fmap proxRatio bins
    writeFile (fname++"-se") $ unlines $ zipWith (\s e->show s++"\t"++show e) s e
    renderableToPDFFile (layoutSE (nbins p) s e) 640 480 (fname++"-se.pdf")
    
    renderableToPDFFile 
        (layoutThese plotBinTimeseries (Alex "AA" "AD" "DD" "DA") $ T.sequenceA bins)
        500 500 (fname++"-bins.pdf")

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
