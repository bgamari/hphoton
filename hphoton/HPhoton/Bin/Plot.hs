module HPhoton.Bin.Plot ( plotBins
                        , plotFret
                        , plotFretEff
                        ) where

import Data.Traversable
import qualified Data.Vector.Unboxed as V
import Control.Lens
import Data.Default
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names

import HPhoton.Bin
import HPhoton.Types
import HPhoton.Fret

plotBins :: Clock -> V.Vector Time -> RealTime -> String -> Colour Double -> Plot RealTime Int
plotBins clk times binWidth label color=
  let binWidth' = realTimeToTime clk binWidth
      bins = V.map (\(t,c)->(timeToRealTime clk t, c))
             $ binWithBounds binWidth' times
  in plotBins' bins label color

plotBins' :: V.Vector (RealTime, Int) -> String -> Colour Double -> Plot RealTime Int
plotBins' bins label color =
  let points = plot_points_title .~ label
             $ plot_points_style .~ plusses 1 0.1 (opaque color)
             $ plot_points_values .~ V.toList bins
             $ def
  in toPlot points

plotFretEff' :: Fret (V.Vector (RealTime, Int)) -> Gamma -> Plot RealTime ProxRatio
plotFretEff' bins gamma =
  let f (t,a) (_,d) = (t, fretEfficiency gamma $ Fret { fretA=realToFrac a+5, fretD=realToFrac d+5 })
      fretEffs = V.zipWith f (fretA bins) (fretD bins)
  in toPlot $ plot_points_title .~ "FRET Efficiency"
            $ plot_points_style .~ plusses 1 0.1 (opaque black)
            $ plot_points_values .~ V.toList fretEffs
            $ def

plotFretEff :: Clock -> Fret (V.Vector Time) -> RealTime -> Gamma -> Plot RealTime FretEff
plotFretEff clk times binWidth =
  let binWidth' = realTimeToTime clk binWidth
      bins = fmap ( V.map (\(t,c)->(timeToRealTime clk t, c))
                  . binWithBounds binWidth'
                  ) times
  in plotFretEff' bins

plotFret :: Clock -> Fret (V.Vector Time) -> RealTime -> [Plot RealTime Int]
plotFret clk times binWidth =
  let binWidth' = realTimeToTime clk binWidth
      bins = fmap ( V.map (\(t,c)->(timeToRealTime clk t, c))
                  . binWithBounds binWidth'
                  ) times
  in [ plotBins' (fretA bins) "Acceptor" red
     , plotBins' (fretD bins) "Donor" green
     ]
