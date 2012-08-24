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

import           Data.IORef
import           Control.Concurrent                 
import           Control.Concurrent.ParallelIO
import           HPhoton.FpgaTimetagger
import           HPhoton.Utils
import           HPhoton.Types hiding (Freq)
import           System.Console.CmdArgs
import           Text.Printf

type Freq = Double

data FitArgs = FitArgs { chain_length     :: Int
                       , number_chains    :: Int
                       , sample_every     :: Int
                       , burnin_length    :: Int
                       , all_chains       :: Bool
                       , model            :: Maybe FilePath
                       , file             :: FilePath
                       , output           :: FilePath
                       , channel          :: Int
                       , clockrate        :: Freq
                       , short_cutoff     :: RealTime
                       , verbose          :: Bool
                       }
             deriving (Data, Typeable, Show)
       
fitArgs = FitArgs
    { file = "" &= typFile &= argPos 0
    , clockrate = 128e6 &= groupname "Input"
                        &= typ "FREQ"
                        &= help "Instrument clockrate (default=128 MHz)"
    , channel = 0 &= groupname "Input"
                  &= help "Channel to fit"
    , short_cutoff = 1e-6 &= typ "TIME"
                          &= groupname "Input"
                          &= help "Discard interarrival times smaller than TIME (default=1 us)"
    , model = Nothing &= typFile
                      &= groupname "Model"
                      &= help "Model file"

    , all_chains = False &= groupname "Output"
                         &= help "Show statistics from all chains"
    , output = "" &= groupname "Output"
                  &= help "Output model to file"
    , verbose = False &= groupname "Output"
                      &= help "Display status updates during chain run"
    
    , chain_length = 100 &= help "Length of Markov chain (default=100)"
                         &= name "l"
                         &= groupname "Sampler"
    , number_chains = 8 &= help "Number of chains to run (default=8)"
                        &= groupname "Sampler"
    , sample_every = 5 &= help "Number of steps to skip between sampling parameters (default=5)"
                       &= groupname "Sampler"
    , burnin_length = 40 &= help "Number of steps to allow chain to burn-in for (default=40)"
                         &= groupname "Sampler"
    }
    &= program "fit-interarrivals"
    &= summary "Fit interarrival times to a mixture of exponential distributions."
    &= details [ "The fitter utilizes a Gibbs sampling approach and can be configured"
               , "to fit either standard or power exponential distributions to photon"
               , "arrival time data. For the time being, the program requires that "
               , "this data be in Goldner lab's FPGA timetagger record format."
               , ""
               , ""
               , " Models"
               , "==========="
               , ""
               , "By default, the program will fit to a two-component exponential model"
               , "with,"
               , ""
               , "  * a low-rate (λ=50 initially) exponential component modelling"
               , "    background"
               , ""
               , "  * a high-rate (λ=5000 initially) stretched exponential"
               , "    (β=1 initially) component modelling fluorescence from diffusing"
               , "    fluorophores"
               , ""
               , "All parameters (λ in the case of a standard exponential and λ and β"
               , "in the stretched case) in addition to component weighing factors"
               , "are allowed to vary during the fit."
               , ""
               , "While this has been found to work well for donor-only FRET data,"
               , "any arbitrary model can be specified with the use of a model file"
               , "(given by the --model option). This is a standard text file listing"
               , "the model components and initial parameters. For instance, the"
               , "default model is given by,"
               , ""
               , "  [ (0.7, Exp 50)"
               , "  , (0.3, StretchedExp 5000 1)"
               , "  ]"
               , ""
               , ""
               , " Sampler"
               , "============"
               , ""
               , "By default, the program will evaluate 8 sampler chains of 100 samples"
               , "each to provide robust estimates of distribution parameters and"
               , "variances on these quantities. If the quality of the produced variance"
               , "estimates are less useful, one can reduce the number of chains with "
               , "the --number-chains option. It is recommended that the chain length "
               , "and burn-in time are not reduced to ensure convergence."
               , ""
               , "Because fits involving many chains can be time-consuming, it recommended"
               , "that the calculation be parallelized. This can be accomplished by"
               , "passing the '+RTS -N n -RTS' argument on the command line, replacing"
               , "'n' with the number of cores to use."
               , ""
               , ""
               , " Afterpulsing correction"
               , "============================"
               , "Often data from single-photon sensitive detectors will be afflicted"
               , "with non-Poissonian afterpulsing events. These artifacts can severely"
               , "bias a fit if left uncorrected. The easiest way to avoid this is to"
               , "cut off short inter-arrival time events by setting the --short-cutoff"
               , "option. The default of 1 microsecond appears to be a reasonable setting"
               , "for our detectors."
               ]

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
          --, (0.1, FixedExp 445e3 0.77)
          , (0.3, StretchedExp 4000 1)
          ]

showExponential :: Exponential -> String
showExponential (FixedExp lambda beta) =
    printf "Fixed Exponential: λ=%1.2e β=%1.2e" lambda beta
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
                 | Running Int LogFloat
                 | Finished
                 deriving (Show, Eq)

statusWorker :: [(Int, IORef ChainStatus)] -> IO ()
statusWorker chains = do
    chains' <- forM chains $ \(i,status)->do s <- readIORef status
                                             return (i,s)
    hPutStr stderr "\n\n"
    forM_ chains' $ \(i,status)->do
        case status of
            Running nIter score -> do
                hPutStr stderr $ printf "%3d: %3d iterations remaining" i nIter
                --hPutStr stderr $ printf "  score=%8f" (logFromLogFloat score :: Double)
                hPutStr stderr "\n"
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

main = do
  fargs <- cmdArgs fitArgs
  recs <- readRecords $ file fargs
  let jiffy = 1 / clockrate fargs
      samples = V.filter (>short_cutoff fargs)
                $ V.map ((jiffy*) . realToFrac)
                $ timesToInterarrivals
                $ strobeTimes recs (argsChannel fargs)
             :: V.Vector Sample

  params <- VB.fromList <$> case model fargs of
      Nothing -> return initial
      Just f  -> read <$> readFile f

  scoreVars <- forM [1..number_chains fargs] $ \i->do 
      var <- newIORef Waiting
      return (i, var)
  when (verbose fargs) $ void $ forkIO $ statusWorker scoreVars
  chains <- parallelInterleaved
            $ map (\(i,var)->runChain fargs params samples var) scoreVars

  when (all_chains fargs) $
      forM_ (zip [1..] chains) $ \(i,chain)->do
          printf "\n\nChain %d\n" (i::Int)
          putStr $ execWriter $ showChainStats chain
  
  putStr "\n\nAll chains\n"
  putStr $ execWriter $ showChainStats $ concat chains
  
  let paramSample = last $ last chains -- TODO: Correctly average
  let mlScore = maxLikelihoodScore paramSample samples
  printf "Score: %1.2e\n" (logFromLogFloat mlScore :: Double)
  printf "Score/sample: %1.2e\n" (logFromLogFloat $ mlScore / realToFrac (V.length samples) :: Double)

  when (length (output fargs) > 0)
      $ withFile (output fargs) WriteMode $ \f->do
          hPrint f $ VB.toList paramSample

-- | Parameter samples of a chain
type Chain = [ComponentParams]

runChain :: FitArgs -> ComponentParams -> V.Vector Sample
         -> IORef ChainStatus -> IO Chain
runChain fargs params samples chainN =
    withSystemRandom $ \mwc->
        sampleFrom mwc (runChain' fargs params samples chainN) :: IO Chain

runChain' :: FitArgs -> ComponentParams -> V.Vector Sample
          -> IORef ChainStatus -> RVarT IO Chain
runChain' fargs params samples scoreVar = do
    assignments0 <- lift $ updateAssignments samples params
    let f :: Int -> (ComponentParams, Assignments)
          -> RVarT IO ((ComponentParams, Assignments), ComponentParams)
        f nIter (params, a) = do
            a' <- lift $ updateAssignments samples params
            let params' = estimateWeights a'
                        $ paramsFromAssignments samples (VB.map snd params) a'
                l = scoreAssignments samples params a'
            lift $ writeIORef scoreVar $ Running nIter l
            return ((params', a'), params')
    steps <- replicateM' (chain_length fargs) f (params, assignments0)

    let paramSamples :: Chain
        paramSamples = takeEvery (sample_every fargs)
                       $ drop (burnin_length fargs) steps
    lift $ writeIORef scoreVar Finished
    return paramSamples

replicateM' :: Monad m => Int -> (Int -> a -> m (a,b)) -> a -> m [b]
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f 1 a >>= return . (:[]) . snd
replicateM' n f a = do (a',b) <- f n a
                       rest <- replicateM' (n-1) f a'
                       return (b:rest)

takeEvery :: Int -> [a] -> [a]
takeEvery n xs | length xs < n = []
               | otherwise     = head xs : takeEvery n (drop n xs)
          