import HPhoton.BayesBurstFind2
import HPhoton.Types
import HPhoton.Utils
import HPhoton.FpgaTimetagger
import Control.Monad
import qualified Data.Vector.Unboxed as V
import Data.Number.LogFloat hiding (realToFrac)
import Data.Number.LogFloat.Vector
import Data.Random
import Data.Random.Distribution.Exponential
import System.Random.MWC (withSystemRandom)
import System.IO
import Text.Printf

dumpPhotons :: V.Vector (Prob, Prob) -> Handle -> IO ()
dumpPhotons a h = do
  let f (idx,(burst,bg)) = hPrintf h "%5d      %1.2e        %1.2e\n"
                           idx (realToFrac burst::Double) (realToFrac bg::Double)
  V.mapM_ f $ V.indexed a
  
  
-- * Testing
testParams = Params { pProbBurst = 1/3
                    , pTauBurst = 200
                    , pTauBG = 1000
                    , pProbBurstStickiness = 1 - 1e-3
                    , pProbBGStickiness    = 1 - 1e-3
                    }
testDts = V.replicate 50 1000 V.++ V.replicate 50 200 V.++ V.replicate 50 1000 :: V.Vector Time
randomDts :: RVar (V.Vector Time)
randomDts = do
  a <- V.replicateM 50 (exponential $ 1000 :: RVar Double)
  b <- V.replicateM 50 (exponential $ 200  :: RVar Double)
  c <- V.replicateM 50 (exponential $ 1000 :: RVar Double)
  return $ V.concat $ map (V.map round) [a, b, c]

testMain = withSystemRandom $ \mwc->do
  dts <- runRVar randomDts mwc
  let pbs = computeProbBs params dts
  let params' = take 10 $ iterate (em dts hypers) testParams
  mapM_ print params'
  withFile "h" WriteMode $ dumpPhotons $ computeProbBs (last params') dts
  
  
-- * Run against file
jiffy = 1/128e6
realRateToTau rate = round $ 1/(rate*jiffy)
bg_rate = 250
burst_rate = 2000
params = Params { pProbBurst = 0.4
                , pTauBurst = realRateToTau burst_rate
                , pTauBG = realRateToTau bg_rate
                , pProbBurstStickiness = 1 - 1e-9
                , pProbBGStickiness = 1 - 1e-9
                }
hypers = HyperParams { hpBGStickiness = (10, 1)
                     , hpBurstStickiness = (10, 1)
                     }

fileMain = do
  rs <- readRecords "hi.timetag"
  let times = strobeTimes rs Ch0
      dts = timesToInterarrivals times
      dur = realToFrac $ V.last times - V.head times
  print params
  print $ dur / realToFrac (pTauBG params)
  print $ dur * jiffy
  print $ realToFrac (V.length dts) / (jiffy * dur)
  
  let pbs = computeProbBs params dts
  let params' = take 10 $ iterate (em dts hypers) params
  mapM_ print params'
  withFile "h" WriteMode $ dumpPhotons $ V.take 10000 $ computeProbBs (last params') dts
  
main = testMain
