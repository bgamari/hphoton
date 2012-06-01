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
import System.Random.MWC (withSystemRandom)                 

import Numeric.MixtureModel.Exponential

import Statistics.Sample (meanVariance)
import Data.List (transpose)

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

longTime = 5e-2

histPlot :: V.Vector Sample -> Plot Sample Double
histPlot xs = histToNormedLinesPlot
              $ plot_hist_bins     ^= 4000
              $ plot_hist_values   ^= [V.toList xs]
              $ plot_hist_range    ^= Just (0, longTime)
              $ plot_hist_no_zeros ^= True
              $ defaultPlotHist

functionPlot :: (RealFrac x, Enum x) => Int -> (x, x) -> (x -> y) -> Plot x y
functionPlot n (a,b) f =
  let xs = [a,a+(b-a)/realToFrac n..b]
  in toPlot $ plot_lines_values ^= [map (\x->(x,f x)) xs]
            $ plot_lines_style .> line_color ^= opaque red
            $ defaultPlotLines

jiffy = 1/128e6

showExponential :: Exponential -> String
showExponential (Exp lambda) = "Exponential: λ="++show lambda
showExponential (StretchedExp lambda beta) =
    "Power exponential: λ="++show lambda++" β="++show beta

showStats :: [Double] -> String
showStats v =
    let (mean,var) = meanVariance $ V.fromList v
    in show mean++" ± "++show var

main = do
  [fname] <- getArgs
  recs <- readRecords fname
  let samples = V.filter (>1e-6)
                $ V.map ((jiffy*) . realToFrac)
                $ timesToInterarrivals
                $ strobeTimes recs Ch0
             :: V.Vector Sample

  withSystemRandom $ \mwc->do
  assignments0 <- sampleFrom mwc $ updateAssignments samples initial
  print $ paramsFromAssignments samples (VB.map snd initial) assignments0

  let f :: (ComponentParams, Assignments) -> RVarT IO (ComponentParams, Assignments)
      f (params, a) = do
        a' <- lift $ updateAssignments samples params
        let params' = estimateWeights a' $ paramsFromAssignments samples (VB.map snd params) a'
        lift $ print (logFromLogFloat $ likelihood samples params a' :: Double)
        return (params', a')
  steps <- sampleFrom mwc $ replicateM' 400 f (initial, assignments0)
  let (params, assignments) = last steps
  let paramSamples = transpose $ takeEvery 2 $ drop 50 $ map (VB.toList . fst) steps
  forM_ (zip [0..] paramSamples) $ \(i,component)->do
      let (w,p) = unzip component
      putStrLn $ "\nComponent "++show i
      putStrLn $ "  Last Params: "++showExponential (last p)
      putStrLn $ "  Weight:      "++showStats w
      putStrLn $ "  <τ>:         "++showStats (map tauMean p)
      putStrLn $ "  <τ²>:        "++showStats (map tauVariance p)

  let dist x = sum $ map (\(w,p)->w * realToFrac (prob p x)) $ VB.toList params
      layout = layout1_plots ^= [ Left $ histPlot samples
                                , Left $ functionPlot 10000 (1e-7,longTime) dist
                                ]
             $ (layout1_left_axis .> laxis_generate) ^= autoScaledLogAxis defaultLogAxis
             $ defaultLayout1
  print params
  renderableToPDFFile (toRenderable layout) 640 480 "hi.pdf"

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m [a]
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a >>= return . (:[])
replicateM' n f a = do b <- f a
                       rest <- replicateM' (n-1) f b
                       return (b:rest)

takeEvery :: Int -> [a] -> [a]
takeEvery n xs | length xs < n = []
               | otherwise     = head xs : takeEvery n (drop n xs)
