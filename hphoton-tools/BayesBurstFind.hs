import HPhoton.BurstIdent.Bayes
import HPhoton.Types
import HPhoton.Fret
import HPhoton.Bin
import HPhoton.Utils
import Options.Applicative
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
                           , clockrate :: Freq
                           , burst_length :: Int
                           , beta_thresh :: Double
                           } 
               deriving (Show)

burstFind :: Parser BurstFind
burstFind = BurstFind
    <$> argument Just ( help "Input file" <> action "file" )
    <*> option ( long "bg-rate" <> short 'b' <> value 1000
              <> help "Background count rate (Hz)" )
    <*> option ( long "burst-rate" <> short 'B' <> value 4000
              <> help "Burst count rate (Hz)" )
    <*> option ( long "clockrate" <> short 'c' <> value (round (128e6 :: Double))
              <> help "Clock period (s)" )
    <*> option ( long "min-length" <> short 'l' <> value 10
              <> help "Minimum burst length" )
    <*> option ( long "odds-thresh" <> short 'o' <> value 2
              <> help "Acceptance threshold on the Bayes factor" )

main :: IO ()
main = do let opts = info (helper <*> burstFind)
                          ( fullDesc <> progDesc "Bayesian fluorescence burst identification" )
          args <- execParser opts
          let realRateToTau rate = round $ realToFrac (clockrate args) / rate
              mp = ModelParams { mpWindow = burst_length args
                               , mpProbB = 0.05
                               , mpTauBg = realRateToTau $ bg_rate args
                               , mpTauBurst = realRateToTau $ burst_rate args
                               }

          d <- readRecords (fname args)
          let clk = clockFromFreq $ clockrate args
          let fret = Fret { fretA = strobeTimes d Ch0, fretD = strobeTimes d Ch1 }
              times = combineChannels [fretA fret, fretD fret]

          let dts = timesToInterarrivals times
              duration = (jiffy clk * fromIntegral (V.last times - V.head times))
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
                         counts = fmap (map V.length . spansPhotons cspans) fret
                     printf "Average %f photons/burst\n"
                       (realToFrac nBurst / realToFrac (length cspans) :: Double)
       
                     let printSpan (start,end) counts =
                           printf "%9u\t%9u\t%4u\t%4u" start end (fretA counts) (fretD counts)
                     writeFile (fname args) $ unlines
                       $ zipWith printSpan cspans (flipFrets counts)
                     

