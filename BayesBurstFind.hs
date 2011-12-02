{-# LANGUAGE DeriveDataTypeable #-}

import HPhoton.BayesBurstFind
import HPhoton.Types
import HPhoton.Bin
import System.Console.CmdArgs
import System.IO
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import Data.Vector.Algorithms.Merge (sort)
import HPhoton.FpgaTimetagger
import Text.Printf
import Data.List (foldl')

type RealTime = Double

data BurstFind = BurstFind { fname :: FilePath
                           , bg_rate :: RealTime
                           , burst_rate :: RealTime
                           , jiffy :: RealTime
                           , burst_length :: Int
                           , beta_thresh :: Double
                           } deriving (Show, Data, Typeable)

burstfind = BurstFind { fname = def &= typFile &= argPos 0
                      , bg_rate = 1000 &= help "Background count rate (Hz)"
                      , burst_rate = 4000 &= help "Burst count rate (Hz)"
                      , jiffy = (1/128e6) &= help "Clock period (s)"
                      , burst_length = 10 &= help "Minimum burst length"
                      , beta_thresh = 2 &= help "Acceptance threshold on beta"
                      }
                      &= summary "Bayesian Burst Find"

data CompressSpansState = CSpansState { startT :: Time
                                      , lastT  :: Time
                                      , result :: [(Time,Time)]
                                      } deriving Show

-- | Reduce a list of times to a list of (startTime, endTime) spans
compressSpans :: Time -> [Time] -> [(Time, Time)]
compressSpans fuzz ts =
        let f :: CompressSpansState -> Time -> CompressSpansState
            f s t  | t - lastT s <= fuzz  = s {lastT=t}
                   | otherwise = s {startT=t, lastT=t, result=(startT s,lastT s):result s}
            s = CSpansState {startT= -1, lastT= -1, result=[]}
            CSpansState _ _ compressed = foldl' f s ts
        in if null compressed then []
                              else tail $ reverse compressed 

-- | Combine multiple timestamp channels
combineChannels :: [V.Vector Time] -> IO (V.Vector Time)
combineChannels chs = do stamps <- V.thaw $ V.concat chs
                         sort stamps
                         stamps' <- V.freeze stamps
                         return stamps'

main = do args <- cmdArgs burstfind
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
                         cspans' = filter (\(a,b)->(b-a) > 15) cspans
                     printf "Average %f photons/burst\n" (realToFrac (V.length bursts) / realToFrac (length cspans) :: Double)

                     f <- openFile (fname args++".spans") WriteMode
                     mapM_ (uncurry $ hPrintf f "%9u\t%9u\n") cspans
                     hClose f

