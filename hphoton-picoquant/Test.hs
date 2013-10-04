{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DataKinds, ExistentialQuantification #-}

import Deconvolve
import Data.Csv as Csv
import Data.Char
import Data.Default
import Control.Applicative
import qualified Data.ByteString.Lazy as LBS
import Control.Lens       
import           Data.Vector (Vector)
import qualified Data.Vector as V
import Linear       
import Linear.V
{-
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart
import Data.Colour
import qualified Data.Colour.Names as C
-}

data Pair a b = Pair !a !b
              deriving (Show, Read, Eq, Ord)

instance Field1 (Pair a b) (Pair a' b) a a' where
    _1 k (Pair a b) = indexed k (0::Int) a <&> \a'-> Pair a' b
instance Field2 (Pair a b) (Pair a b') b b' where
    _2 k (Pair a b) = indexed k (0::Int) b <&> \b'-> Pair a b'
    
readCorr :: FilePath -> IO (Either String (V.Vector (Int,Double)))
readCorr fname =
    Csv.decodeWith decodeOpts False <$> LBS.readFile fname
  where decodeOpts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral $ ord '\t' }

adjustCorr :: V.Vector (Int,Double) -> V.Vector (Pair Int Double)
adjustCorr xs = V.take 3000
              $ V.map (\(x,y)->(Pair (x-x0) y)) xs
    where (x0,_) = V.head xs

shiftPsf :: V.Vector a -> V.Vector (V.Vector a)
shiftPsf v = V.generate (V.length v) shift
  where shift i = V.drop i v V.++ V.take i v

vectorPlease :: forall k a. Dim k => Vector a -> V k a
vectorPlease v = case fromVector v of Just v' -> v'

main = do
    Right irf <- fmap adjustCorr <$> readCorr "water.txt"
    Right d <- fmap adjustCorr <$> readCorr "frnad.txt"
    
    let d' = vectorPlease $ V.map (view _2) d :: Dim k => V k Double
        irf' = fmap vectorPlease $ shiftPsf $ V.map (view _2) irf
             :: (Dim k1) => Vector (V k1 Double)
        deconv = head $ drop 10
                 $ richardsonLucy (vectorPlease irf') d'
    print deconv
    {-
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
    -}
