{-# LANGUAGE PackageImports #-}

import Control.Lens
import Data.Default       
import System.Environment
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import HPhoton.Types
import HPhoton.Bin
import HPhoton.IO.FpgaTimetagger
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Plot.Histogram
import Numeric.Histogram
import Text.Printf

clk = clockFromFreq $ round 128e6

main = do
    f:_ <- getArgs
    recs <- readRecords f
    let ts = strobeTimes recs Ch0
        width = realTimeToTime clk 1e-4
        bins = bin' width (VU.toList ts) Nothing (fromIntegral $ VU.head ts `quot` width) 0
        --bins = bin width ts
        hist = plot_hist_bins    .~ 150
             $ plot_hist_values  .~ (V.fromList $ map realToFrac bins :: V.Vector Double)
             $ plot_hist_range   .~ Just (0, 150)
             $ defaultPlotHist
        layout = layout1_plots   .~ [Left $ histToPlot hist]
               $ def

    putStrLn "hi"
    mapM_ (\((a,b),n)->putStr $ printf "%1.3e %1.3e    %6d\n" (a::Double) b n)
        $ V.toList $ histValues 0 500 500 $ map realToFrac bins
    --renderableToPNGFile (toRenderable layout) 640 480 "f.png"
    return ()
