module Graphics.Rendering.Chart.Simple.Histogram ( simpleHist
                                                 , generateAxisData
                                                 ) where

import Data.List (foldl1')
import Control.Lens
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Plot.Histogram
import qualified Data.Vector as V
import Data.Default

minMax :: Ord a => [a] -> (a,a)
minMax = foldl1' (\(a,b) (x,y)->(min a x, max b y)) . map (\x->(x,x))

generateAxisData :: (PlotValue x, Show x, Num x)
                 => Int -> AxisFn x
generateAxisData nbins = generateAxisData' nbins . minMax

generateAxisData' :: (PlotValue x, Show x, Num x)
                  => Int -> (x,x) -> AxisData x
generateAxisData' nbins (min,max) =
    makeAxis show (ticks, grids, labels)
 where ticks = let dx = toValue (max - min) / realToFrac nbins
               in [fromValue $ toValue min + i*dx | i <- [0..realToFrac nbins]]
       grids = []
       labels = []

chart :: (Ord x, Show x, RealFrac x, PlotValue x) => Int -> V.Vector x -> Layout x Int
chart _ xs | V.length xs < 2 = error "Can't histogram (nearly) empty list"
chart nbins xs = layout
        where (min,max) = minMax $ V.toList xs
              hist = plot_hist_values  .~ xs
                     $ plot_hist_range .~ Just (min,max)
                     $ plot_hist_bins  .~ nbins
                     $ defaultPlotHist
              layout = layout_plots .~ [histToPlot hist]
                     $ (layout_x_axis .> laxis_generate) .~
                           const (generateAxisData' nbins (min,max))
                     $ def

simpleHist :: FilePath -> Int -> V.Vector Double -> IO ()
simpleHist fname nbins xs =
  renderableToFile def fname (toRenderable $ chart nbins xs) >> return ()
