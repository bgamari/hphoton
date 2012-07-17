module Graphics.Rendering.Chart.Simple.Histogram ( simpleHist
                                                 , generateAxisData
                                                 ) where

import Data.List (foldl1')       
import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Plot.Histogram

minMax :: Ord a => [a] -> (a,a)
minMax = foldl1' (\(a,b) (x,y)->(min a x, max b y)) . map (\x->(x,x))

generateAxisData :: (PlotValue x, Show x, Num x)  => Int -> AxisFn x
generateAxisData nbins = generateAxisData' nbins . minMax

generateAxisData' :: (PlotValue x, Show x, Num x) => Int -> (x,x) -> AxisData x
generateAxisData' nbins (min,max) = makeAxis show (ticks, grids, labels)
        where ticks = let dx = toValue (max - min) / realToFrac nbins
                      in [fromValue $ toValue min + i*dx | i <- [0..realToFrac nbins]]
              grids = []
              labels = []

chart :: (Ord x, Show x, RealFrac x, PlotValue x) => Int -> [x] -> Layout1 x Int
chart _ xs | length xs < 2 = error "Can't histogram (nearly) empty list"
chart nbins xs = layout
        where (min,max) = minMax xs
              hist = plot_hist_values  ^= xs
                     $ plot_hist_range ^= Just (min,max)
                     $ plot_hist_bins  ^= nbins
                     $ defaultPlotHist
              layout = layout1_plots ^= [Left $ histToPlot hist]
                     $ (layout1_bottom_axis .> laxis_generate) ^=
                           const (generateAxisData' nbins (min,max))
                     $ defaultLayout1
              
simpleHist :: FilePath -> Int -> [Double] -> IO ()
simpleHist fname nbins xs =
  renderableToPNGFile (toRenderable $ chart nbins xs) 640 480 fname >> return ()
                            
