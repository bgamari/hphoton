module HPhoton.Bin.Plot ( plotBins
                        , plotFret
                        ) where

import Data.Traversable
import qualified Data.Vector.Unboxed as V
import Data.Accessor
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names       

import HPhoton.Bin       
import HPhoton.Types       
import HPhoton.Fret.Types       

plotBins :: Clock -> V.Vector Time -> RealTime -> String -> Colour Double -> Plot RealTime Int
plotBins clk times binWidth label color=
  let binWidth' = realTimeToTime clk binWidth
      bins = V.map (\(t,c)->(timeToRealTime clk t, c))
             $ binTimesWithTimes times binWidth'
      points = plot_points_title ^= label
             $ plot_points_style ^= plusses 1 0.1 (opaque color)
             $ plot_points_values ^= V.toList bins
             $ defaultPlotPoints
  in toPlot points

plotFret :: Clock -> Fret (V.Vector Time) -> RealTime -> [Plot RealTime Int]
plotFret clk times binWidth =
  [ plotBins clk (fretA times) binWidth "Acceptor" red
  , plotBins clk (fretD times) binWidth "Donor" green
  ]

