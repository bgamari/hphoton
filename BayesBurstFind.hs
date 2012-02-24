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

main = do args <- cmdArgs burstFind
          let realRateToTau rate = round $ 1/(rate*jiffy args)
              mp = ModelParams { mpWindow = burst_length args
                               , mpProbB = 0.05
                               , mpTauBg = realRateToTau (bg_rate args)
                               , mpTauBurst = realRateToTau (burst_rate args)
                               }

          rs <- readRecords (fname args)
          let (stampsA, stampsD) = (strobeTimes rs Ch0, strobeTimes rs Ch1)
              times = combineChannels [stampsA, stampsD]

          let dts = timesToInterarrivals times
              duration = (jiffy args * fromIntegral (V.last times - V.head times))
          printf "%d photons\n" (V.length times)
          printf "Timestamp range %u..%u : %4.2e seconds\n" (V.head times) (V.last times) duration
          printf "Average rate %1.3f photons/second\n" $ (fromIntegral $ V.length dts) / duration
          print mp

          let burstTimes = V.map (times!) $ findBurstPhotons mp (beta_thresh args) dts
              nBurst = V.length burstTimes
          if nBurst == 0
             then putStrLn "No bursts found"
             else do printf "Found %u burst photons\n" nBurst
                     let cspans = V.toList $ compressSpans (40*mpTauBurst mp) burstTimes
                         aCounts = map V.length $ spansPhotons stampsA cspans
                         dCounts = map V.length $ spansPhotons stampsD cspans
                     printf "Average %f photons/burst\n"
                       (realToFrac nBurst / realToFrac (length cspans) :: Double)
       
                     let printSpan (start,end) aCount dCount =
                           printf "%9u\t%9u\t%4u\t%4u" start end aCount dCount
                     writeFile (fname args) $ unlines
                       $ zipWith3 printSpan cspans aCounts dCounts
                     

