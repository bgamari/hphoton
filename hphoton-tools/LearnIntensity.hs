{-# LANGUAGE PackageImports #-}
import System.Environment       
import qualified Data.Vector.Unboxed as V
import HPhoton.Types
import HPhoton.Bin
import HPhoton.FpgaTimetagger
import "data-accessor" Data.Accessor       
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
        bins = bin' width (V.toList ts) Nothing (fromIntegral $ V.head ts `quot` width) 0
        --bins = bin width ts
        hist = plot_hist_bins    ^= 150
             $ plot_hist_values  ^= (map realToFrac bins :: [Double])
             $ plot_hist_range   ^= Just (0, 150)
             $ defaultPlotHist
        layout = layout1_plots ^= [Left $ histToPlot hist]
               $ defaultLayout1

    putStrLn "hi"
    mapM_ (\((a,b),n)->putStr $ printf "%1.3e %1.3e    %6d\n" (a::Double) b n)
        $ histValues 0 500 500 $ map realToFrac bins
    --renderableToPNGFile (toRenderable layout) 640 480 "f.png"
    return ()
