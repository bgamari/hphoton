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

plotBins :: Clocked (V.Vector Time) -> RealTime -> String -> Colour Double -> Plot RealTime Int
plotBins times binWidth label color=
  let binWidth' = unClocked $ realTimeToTime (jiffy times) binWidth
      bins = V.map (\(t,c)->(timeToRealTime $ Clocked (freq times) t, c))
             $ binTimesWithTimes (unClocked times) binWidth'
      points = plot_points_title ^= label
             $ plot_points_style ^= plusses 1 0.1 (opaque color)
             $ plot_points_values ^= V.toList bins
             $ defaultPlotPoints
  in toPlot points

plotFret :: Clocked (Fret (V.Vector Time)) -> RealTime -> [Plot RealTime Int]
plotFret times binWidth =
  [ plotBins (fretA $ sequenceA times) binWidth "Acceptor" red
  , plotBins (fretD $ sequenceA times) binWidth "Donor" green
  ]
