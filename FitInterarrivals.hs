import System.Environment (getArgs)                
import Data.Number.LogFloat                 hiding (realToFrac)
import Data.Random.Lift
import qualified Data.Vector.Unboxed as V                
import Control.Applicative                
import Control.Monad                
import           Data.Random                 
import           Data.Random.Distribution.Beta
import           Data.Random.Distribution.Categorical
import System.Random.MWC (create)                 

import Numeric.MixtureModel.Exponential

import           HPhoton.FpgaTimetagger
import HPhoton.Utils
import Data.Accessor       
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Plot.Histogram       
import Data.Colour
import Data.Colour.Names       
       
initial :: [(Weight, ExpParam)]       
initial = [ (0.5, 50)
          , (0.5, 5000)
          ]

histPlot :: V.Vector Sample -> Plot Sample Double
histPlot xs = plotNormedHist $ plot_hist_bins ^= 40
                             $ plot_hist_values ^= [V.toList xs]
                             $ defaultPlotHist

functionPlot :: (RealFrac x, Enum x) => Int -> (x, x) -> (x -> y) -> Plot x y
functionPlot n (a,b) f =
  let xs = [a,a+(b-a)/realToFrac n..b]
  in toPlot $ plot_lines_values ^= [map (\x->(x,f x)) xs]
            $ plot_lines_style .> line_color ^= opaque red
            $ defaultPlotLines

jiffy = 1/128e6

main = do
  [fname] <- getArgs
  recs <- readRecords fname
  let samples = V.filter (>10e-6)
                $ V.map ((jiffy*) . realToFrac)
                $ timesToInterarrivals
                $ strobeTimes recs Ch0
             :: V.Vector Sample

  mwc <- create
  assignments0 <- sampleFrom mwc $ updateAssignments' samples (V.fromList initial)
  print $ paramsFromAssignments samples 2 assignments0
  let f :: Assignments -> RVarT IO Assignments
      f a = do
        a' <- lift $ updateAssignments samples 2 a
        let params = estimateWeights a' $ paramsFromAssignments samples 2 a'
        lift $ print (logFromLogFloat $ likelihood samples params a' :: Double)
        return a'
  assignments <- sampleFrom mwc
                 $ replicateM' 40 f assignments0
  print $ paramsFromAssignments samples 2 assignments

  let params = estimateWeights assignments
               $ paramsFromAssignments samples 2 assignments
      dist x = sum $ map (\(w,p)->w * realToFrac (expProb p x)) $ V.toList params
  let layout = layout1_plots ^= [ Left $ histPlot samples
                                , Left $ functionPlot 100 (1e-6,1e-2) dist
                                ]
             -- $ (layout1_left_axis .> laxis_generate) ^= autoScaledLogAxis defaultLogAxis
             $ defaultLayout1
  print params
  renderableToPDFFile (toRenderable layout) 640 480 "hi.pdf"

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f

