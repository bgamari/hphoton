{-# LANGUAGE DeriveDataTypeable #-}

import HPhoton.BayesBurstFind
import HPhoton.Types
import HPhoton.Bin
import HPhoton.Utils
import System.Console.CmdArgs
import System.IO
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import Data.Vector.Algorithms.Merge (sort)
import HPhoton.FpgaTimetagger
import Text.Printf
import Data.List (foldl')
import Control.Monad.Trans.State

data BurstFind = BurstFind { fname :: FilePath
                           , bg_rate :: RealTime
                           , burst_rate :: RealTime
                           , jiffy :: RealTime
                           , burst_length :: Int
                           , beta_thresh :: Double
                           } 
               deriving (Show, Data, Typeable)

burstFind = BurstFind { fname = def &= typFile &= argPos 0
                      , bg_rate = 1000 &= help "Background count rate (Hz)"
                      , burst_rate = 4000 &= help "Burst count rate (Hz)"
                      , jiffy = (1/128e6) &= help "Clock period (s)"
                      , burst_length = 10 &= help "Minimum burst length"
                      , beta_thresh = 2 &= help "Acceptance threshold on beta"
                      }
                      &= summary "Bayesian Burst Find"

-- | 'spansPhotons ts spans' returns the photons in a set of spans
spansPhotons :: V.Vector Time -> [(Time,Time)] -> [V.Vector Time]
spansPhotons ts spans = evalState (mapM f spans) ts
  where f :: (Time,Time) -> State (V.Vector Time) (V.Vector Time)
        f (start,end) = do ts <- get
                           let (a,b) = V.span (<=end) $ V.dropWhile (<start) ts
                           put b
                           return a
  
main = do args <- cmdArgs burstFind
          let realRateToTau rate = round $ 1/(rate*jiffy args)
              n = burst_length args
              mp = ModelParams { prob_b = 0.05
                               , tau_bg = realRateToTau (bg_rate args)
                               , tau_burst = realRateToTau (burst_rate args)
                               }

          rs <- readRecords (fname args)
          let (stampsA, stampsD) = (strobeTimes rs Ch0, strobeTimes rs Ch1)
          times <- combineChannels [stampsA, stampsD]

          let dts = V.zipWith (-) (V.tail times) times
              duration = (jiffy args * fromIntegral (V.last times - V.head times))
          printf "%d photons\n" (V.length times)
          printf "Timestamp range %u..%u : %4.2e seconds\n" (V.head times) (V.last times) duration
          printf "Average rate %1.3f photons/second\n" $ (fromIntegral $ V.length dts) / duration
          print mp

          let betas = V.map (\i->(times ! fromIntegral i, beta n dts mp i))
                    $ V.enumFromN (fromIntegral n) (V.length dts-2*n)
              bursts = V.filter (\(t,beta) -> beta > beta_thresh args) betas
              burstTimes = V.map fst bursts 

          if V.length bursts == 0
             then putStrLn "No bursts found"
             else do printf "Found %u burst photons\n" (V.length bursts)
                     let cspans = compressSpans (40*tau_burst mp) (V.toList burstTimes)
                     printf "Average %f photons/burst\n" (realToFrac (V.length bursts) / realToFrac (length cspans) :: Double)

                     fSpans <- openFile (fname args++".spans") WriteMode
                     let printSpan ((start,end), aCount, dCount) = hPrintf fSpans "%9u\t%9u\t%4u\t%4u\n" start end aCount dCount
                     mapM_ printSpan $ zip3 cspans (map V.length $ spansPhotons stampsA cspans) (map V.length $ spansPhotons stampsD cspans)
                     hClose fSpans
       

