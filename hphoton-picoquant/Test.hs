import Deconvolve
import Data.Csv as Csv
import Data.Char
import Data.Default
import Control.Applicative
import qualified Data.ByteString.Lazy as LBS
import Control.Lens       
import           Data.Vector (Vector)
import qualified Data.Vector as V
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart
import Data.Colour
import qualified Data.Colour.Names as C

readCorr :: FilePath -> IO (Either String (V.Vector (Int, Double)))
readCorr fname =
    Csv.decodeWith decodeOpts False <$> LBS.readFile fname
  where decodeOpts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral $ ord '\t' }

adjustCorr :: V.Vector (Int, Double) -> V.Vector (Int, Double)
adjustCorr xs = V.take 3000
              $ V.map (\(x,y)->(x-x0, y)) xs
    where (x0,_) = V.head xs

shiftPsf :: V.Vector a -> V.Vector (V.Vector a)
shiftPsf v = V.generate (V.length v) shift
  where shift i = V.drop i v V.++ V.take i v

main = do
    Right irf <- fmap adjustCorr <$> readCorr "water.txt"
    Right d <- fmap adjustCorr <$> readCorr "frnad.txt"
    
    let deconv = head $ drop 10
                 $ richardsonLucy (shiftPsf $ V.map snd irf) (V.map snd d)
    let plot d = Left $ toPlot $ plot_points_values .~ V.toList d $ def
        plotPlusses color d =
            Left $ toPlot
            $ plot_points_values .~ V.toList d
            $ plot_points_style  .~ plusses 1 0.1 (opaque color)
            $ def
        layout = layout1_title .~ "Hi"
               $ layout1_left_axis . laxis_generate .~ autoScaledLogAxis def
               $ layout1_plots .~
                   [ plotPlusses C.red d
                   , plotPlusses C.blue irf
                   , plotPlusses C.green $ V.zip (V.map fst d) deconv
                   ]
               $ def
    renderableToWindow (toRenderable layout) 640 480
    
