import System.Environment (getArgs)                
import Data.Number.LogFloat                 hiding (realToFrac)
import Data.Random.Lift
import qualified Data.Vector as VB
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
       
initial :: VB.Vector (Weight, Exponential)
initial = VB.fromList 
        $ [ (0.8, Exp 50)
          , (0.2, StretchedExp 5000 1)
          ]

histPlot :: V.Vector Sample -> Plot Sample Double
histPlot xs = histToNormedLinesPlot
              $ plot_hist_bins     ^= 4000
              $ plot_hist_values   ^= [V.toList xs]
              $ plot_hist_range    ^= Just (0, 0.12)
              $ plot_hist_no_zeros ^= True
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
  let samples = V.filter (>1e-6)
                $ V.map ((jiffy*) . realToFrac)
                $ timesToInterarrivals
                $ strobeTimes recs Ch0
             :: V.Vector Sample

  mwc <- create
  assignments0 <- sampleFrom mwc $ updateAssignments samples initial
  print $ paramsFromAssignments samples (VB.map snd initial) assignments0

  let f :: (ComponentParams, Assignments) -> RVarT IO (ComponentParams, Assignments)
      f (params, a) = do
        a' <- lift $ updateAssignments samples params
        let params' = estimateWeights a' $ paramsFromAssignments samples (VB.map snd params) a'
        lift $ print (params', logFromLogFloat $ likelihood samples params a' :: Double)
        return (params', a')
  (params, assignments) <- sampleFrom mwc $ replicateM' 40 f (initial, assignments0)

  print $ paramsFromAssignments samples (VB.map snd params) assignments
  let dist x = sum $ map (\(w,p)->w * realToFrac (prob p x)) $ VB.toList params
      layout = layout1_plots ^= [ Left $ histPlot samples
                                , Left $ functionPlot 10000 (1e-7,1e-1) ((1+) . dist)
                                ]
             $ (layout1_left_axis .> laxis_generate) ^= autoScaledLogAxis defaultLogAxis
             $ defaultLayout1
  print params
  renderableToPDFFile (toRenderable layout) 640 480 "hi.pdf"

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f

