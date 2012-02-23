module Graphics.Rendering.Chart.Simple.Histogram (simpleHist) where

import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Plot.Histogram

chart nbins xs = layout
        where hist = plot_hist_values  ^= [xs]
                     $ plot_hist_range ^= Just (minimum xs, maximum xs)
                     $ defaultPlotHist
              layout = layout1_title ^= "Simple Histogram"
                     $ layout1_plots ^= [Left (plotHist hist)]
                     $ defaultLayout1
              
simpleHist :: FilePath -> Int -> [Double] -> IO ()
simpleHist fname nbins xs =
  renderableToPNGFile (toRenderable $ chart nbins xs) 640 480 fname >> return ()
                            
