{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

import           System.IO
import           Data.Number.LogFloat hiding (realToFrac)
import           Data.Random.Lift
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Writer                 
import           Data.Random
import           Data.Random.Distribution.Beta
import           Data.Random.Distribution.Categorical
import           System.Random.MWC (withSystemRandom, GenIO)

import           Numeric.MixtureModel.Exponential

import           Statistics.Sample (meanVariance)
import           Data.List (transpose)

import           Control.Concurrent                 
import           Control.Concurrent.ParallelIO
import           Data.Accessor
import           Data.Colour
import           Data.Colour.Names
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Plot.Histogram
import           HPhoton.FpgaTimetagger
import           HPhoton.Utils
import           System.Console.CmdArgs
import           Text.Printf

type Freq = Double

data FitArgs = FitArgs { chain_length     :: Int
                       , number_chains    :: Int
                       , sample_every     :: Int
                       , burnin_length    :: Int
                       , plot             :: Bool
                       , model            :: Maybe FilePath
                       , file             :: FilePath
                       , channel          :: Int
                       , clockrate        :: Freq
                       }
             deriving (Data, Typeable, Show)
       
fitArgs = FitArgs { chain_length = 100 &= help "Length of Markov chain"
                  , number_chains = 40 &= help "Number of chains to run"
                  , sample_every = 5 &= help "Number of steps to skip between sampling parameters"
                  , burnin_length = 40 &= help "Number of steps to allow chain to burn-in for"
                  , plot = False &= help "Produce plots showing model components"
                  , model = Nothing &= help "Model file"
                  , file = "" &= typFile &= argPos 0
                  , clockrate = 128e6 &= typ "FREQ" &= help "Instrument clockrate"
                  , channel = 0 &= help "Channel to fit"
                  }
        &= program "fit-interarrivals"
        &= summary "fit-interarrivals"
        &= details ["Fit interarrival times to a mixture of exponential distributions."]

argsChannel :: FitArgs -> Channel
argsChannel (FitArgs {channel=ch}) =
    case ch of
        0         -> Ch0        
        1         -> Ch1
        2         -> Ch2
        3         -> Ch3
        otherwise -> error "Invalid channel"
        
initial :: [(Weight, Exponential)]
initial = [ (0.7, Exp 50)
          , (0.2, StretchedExp 5000 1)
          ]

longTime = 5e-2

histPlot :: (Double, Double) -> V.Vector Sample -> Plot Sample Double
histPlot range xs = histToNormedLinesPlot
              $ plot_hist_bins     ^= 4000
              $ plot_hist_values   ^= [V.toList xs]
              $ plot_hist_range    ^= Just range
              $ plot_hist_no_zeros ^= True
              $ defaultPlotHist

functionPlot :: (RealFrac x, Enum x) => Int -> (x, x) -> (x -> y) -> Plot x y
functionPlot n (a,b) f =
  let xs = [a,a+(b-a)/realToFrac n..b]
  in toPlot $ plot_lines_values ^= [map (\x->(x,f x)) xs]
            $ plot_lines_style .> line_color ^= opaque red
            $ defaultPlotLines

showExponential :: Exponential -> String
showExponential (Exp lambda) =
    printf "Exponential: λ=%1.2e" lambda
showExponential (StretchedExp lambda beta) =
    printf "Power exponential: λ=%1.2e β=%1.2e" lambda beta

showStats :: [Double] -> String
showStats v =
    let (mean,var) = meanVariance $ V.fromList v
    in printf "%1.3e    σ²=%1.3e"  mean var

showChainStats :: Chain -> Writer String ()
showChainStats chain = do
    let components = transpose $ map VB.toList chain
    forM_ (zip [1..] components) $ \(i,c)->do
        let (w,p) = unzip c
        tell $ "\nComponent "++show i++"\n"
        tell $ "  Last Params: "++showExponential (last p)++"\n"
        tell $ "  Weight:      "++showStats w++"\n"
        tell $ "  〈τ〉:         "++showStats (map tauMean p)++"\n"
        tell $ "  〈τ²〉:        "++showStats (map tauVariance p)++"\n"

data ChainStatus = Waiting
                 | Running LogFloat
                 | Finished
                 deriving (Show, Eq)

statusWorker :: [(Int, MVar ChainStatus)] -> IO ()
statusWorker chains = do
    chains' <- forM chains $ \(i,status)->do s <- readMVar status
                                             return (i,s)
    hPutStr stderr "\n\n"
    forM_ chains' $ \(i,status)->do
        case status of
            Running likelihood ->
                hPutStr stderr $ printf "%3d: Likelihood %8f\n"
                                        i (logFromLogFloat likelihood :: Double)
            otherwise -> return ()
    hPutStr stderr
        $ printf "%3d / %3d finished\n"
                 (length $ filter (\(_,s)->s==Finished) chains')
                 (length chains')
    hFlush stderr
    threadDelay 5000000
    if all (\(_,s)->s == Finished) chains'
        then return ()
        else statusWorker chains

plotFit :: V.Vector Sample -> (Double,Double) -> [Double->Double] -> Layout1 Double Double
plotFit samples (a,b) fits =
    layout1_plots ^= [ Left $ histPlot (a,b) samples ]
                     ++ map (Left . functionPlot 1000 (a,b)) fits
    $ (layout1_left_axis .> laxis_generate) ^= autoScaledLogAxis defaultLogAxis
    $ defaultLayout1
    
plotParamSample :: V.Vector Sample -> ComponentParams -> IO ()
plotParamSample samples paramSample = do
  let dist x = sum $ map (\(w,p)->w * realToFrac (prob p x)) $ VB.toList paramSample
  renderableToPDFFile (toRenderable $ plotFit samples (1e-7,longTime) [dist]) 640 480 "all.pdf"
  forM_ (zip [1..] $ VB.toList paramSample) $ \(i,(w,p))->do
      let c x = w * realToFrac (prob p x)
          longTime = 2 * tauMean p
      renderableToPDFFile (toRenderable $ plotFit samples (1e-7,longTime) [dist, c]) 640 480 (printf "component%03d.pdf" (i::Int))

main = do
  fargs <- cmdArgs fitArgs
  recs <- readRecords $ file fargs
  let jiffy = 1 / clockrate fargs
      samples = V.filter (>1e-6)
                $ V.map ((jiffy*) . realToFrac)
                $ timesToInterarrivals
                $ strobeTimes recs (argsChannel fargs)
             :: V.Vector Sample

  params <- VB.fromList <$> case model fargs of
      Nothing -> return initial
      Just f  -> read <$> readFile f

  likelihoodVars <- forM [1..number_chains fargs] $ \i->do 
      var <- newMVar Waiting
      return (i, var)
  forkIO $ statusWorker likelihoodVars
  chains <- parallelInterleaved
            $ map (\(i,var)->runChain fargs params samples var) likelihoodVars

  forM_ (zip [1..] chains) $ \(i,chain)->do
      printf "\n\nChain %d\n" (i::Int)
      putStr $ execWriter $ showChainStats chain
  
  putStr "\n\nChain totals\n"
  putStr $ execWriter $ showChainStats $ concat chains
  
  let paramSample = last $ last chains -- TODO: Correctly average
  when (plot fargs) $ plotParamSample samples paramSample

-- | Parameter samples of a chain
type Chain = [ComponentParams]

runChain :: FitArgs -> ComponentParams -> V.Vector Sample
         -> MVar ChainStatus -> IO Chain
runChain fargs params samples chainN =
    withSystemRandom $ \mwc->
        sampleFrom mwc (runChain' fargs params samples chainN) :: IO Chain

runChain' :: FitArgs -> ComponentParams -> V.Vector Sample
          -> MVar ChainStatus -> RVarT IO Chain
runChain' fargs params samples likelihoodVar = do
    assignments0 <- lift $ updateAssignments samples params
    let f :: (ComponentParams, Assignments)
          -> RVarT IO ((ComponentParams, Assignments), ComponentParams)
        f (params, a) = do
            a' <- lift $ updateAssignments samples params
            let params' = estimateWeights a'
                        $ paramsFromAssignments samples (VB.map snd params) a'
                l = scoreAssignments samples params a'
            lift $ swapMVar likelihoodVar $ Running l
            return ((params', a'), params')
    steps <- replicateM' (chain_length fargs) f (params, assignments0)

    let paramSamples :: Chain
        paramSamples = takeEvery (sample_every fargs)
                       $ drop (burnin_length fargs) steps
    lift $ swapMVar likelihoodVar Finished
    return paramSamples

replicateM' :: Monad m => Int -> (a -> m (a,b)) -> a -> m [b]
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a >>= return . (:[]) . snd
replicateM' n f a = do (a',b) <- f a
                       rest <- replicateM' (n-1) f a'
                       return (b:rest)

takeEvery :: Int -> [a] -> [a]
takeEvery n xs | length xs < n = []
               | otherwise     = head xs : takeEvery n (drop n xs)
