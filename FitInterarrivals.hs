{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

import System.IO
import Data.Number.LogFloat hiding (realToFrac)
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
import Text.Printf       
import System.Console.CmdArgs

data FitArgs = FitArgs { chain_length     :: Int
                       , number_chains    :: Int
                       , sample_every     :: Int
                       , burnin_length    :: Int
                       , file             :: FilePath
                       }
             deriving (Data, Typeable, Show)
       
fitArgs = FitArgs { chain_length = 100 &= help "Length of Markov chain"
                  , number_chains = 200 &= help "Number of chains to run"
                  , sample_every = 5 &= help "Number of steps to skip between sampling parameters"
                  , burnin_length = 40 &= help "Number of steps to allow chain to burn-in for"
                  , file = "" &= help "File to process" &= typFile &= argPos 0
                  }
        &= summary "fit-interarrivals"
        &= details ["Fit interarrival times from mixture of Poisson processes"]

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
showExponential (Exp lambda) =
    printf "Exponential: λ=%1.2e" lambda
showExponential (StretchedExp lambda beta) =
    printf "Power exponential: λ=%1.2e β=%1.2e" lambda beta

showStats :: [Double] -> String
showStats v =
    let (mean,var) = meanVariance $ V.fromList v
    in printf "%1.3e    σ²=%1.3e"  mean var

main = do
  fargs <- cmdArgs fitArgs
  recs <- readRecords $ file fargs
  let samples = V.filter (>1e-6)
                $ V.map ((jiffy*) . realToFrac)
                $ timesToInterarrivals
                $ strobeTimes recs Ch0
             :: V.Vector Sample

  withSystemRandom $ \mwc->do
  assignments0 <- sampleFrom mwc $ updateAssignments samples initial
  print $ paramsFromAssignments samples (VB.map snd initial) assignments0

  let f :: (ComponentParams, Assignments)
        -> RVarT IO ((ComponentParams, Assignments), ComponentParams)
      f (params, a) = do
        a' <- lift $ updateAssignments samples params
        let params' = estimateWeights a'
                      $ paramsFromAssignments samples (VB.map snd params) a'
            l = likelihood samples params a'
        lift $ hPutStr stderr $ printf "Likelihood: %1.5e\n" (logFromLogFloat l :: Double) 
        return ((params', a'), params')
  steps <- sampleFrom mwc $ replicateM' (chain_length fargs) f (initial, assignments0)

  let paramSamples = transpose $ takeEvery (sample_every fargs)
                     $ drop (burnin_length fargs)
                     $ map VB.toList steps
  forM_ (zip [0..] paramSamples) $ \(i,component)->do
      let (w,p) = unzip component
      putStrLn $ "\nComponent "++show i
      putStrLn $ "  Last Params: "++showExponential (last p)
      putStrLn $ "  Weight:      "++showStats w
      putStrLn $ "  <τ>:         "++showStats (map tauMean p)
      putStrLn $ "  <τ²>:        "++showStats (map tauVariance p)

  let dist x = sum $ map (\(w,p)->w * realToFrac (prob p x)) $ VB.toList $ last steps
      layout = layout1_plots ^= [ Left $ histPlot samples
                                , Left $ functionPlot 10000 (1e-7,longTime) dist
                                ]
             $ (layout1_left_axis .> laxis_generate) ^= autoScaledLogAxis defaultLogAxis
             $ defaultLayout1
  renderableToPDFFile (toRenderable layout) 640 480 "hi.pdf"

replicateM' :: Monad m => Int -> (a -> m (a,b)) -> a -> m [b]
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a >>= return . (:[]) . snd
replicateM' n f a = do (a',b) <- f a
                       rest <- replicateM' (n-1) f a'
                       return (b:rest)

takeEvery :: Int -> [a] -> [a]
takeEvery n xs | length xs < n = []
               | otherwise     = head xs : takeEvery n (drop n xs)
