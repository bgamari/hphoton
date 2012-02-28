module HPhoton.BayesBurstFind2 where

import HPhoton.Types
import HPhoton.Utils
import HPhoton.FpgaTimetagger
import qualified Data.Vector.Unboxed as V
import Data.Number.LogFloat hiding (realToFrac)
import Data.Number.LogFloat.Vector
import Data.Random
import Data.Random.Distribution.Exponential
  
data ModelState = ModelState { msBs :: V.Vector Bool }
                deriving (Show, Eq)

type Prob = LogFloat

data HyperParams = HyperParams { hpBurstStickiness :: (Double, Double)
                               , hpBGStickiness :: (Double, Double)
                               }
                   
data Params = Params { pProbBurst :: Prob
                     , pTauBurst :: Time
                     , pTauBG :: Time
                     , pProbBurstStickiness :: Prob
                     , pProbBGStickiness :: Prob
                     }
            deriving (Show, Eq)
                     
-- | `expDist lambda tau` is `P(tau | lambda)`
expDist :: Time -> Time -> Prob
expDist tau t = realToFrac $ 1/tau' * exp (-t' / tau')
  where tau' = realToFrac tau
        t' = realToFrac t
  
type BDist = (LogFloat, LogFloat)
             
-- | Compute P(B_i | tau_i) for all i
computeProbBs :: Params -> V.Vector Time -> V.Vector BDist
computeProbBs params dts =
  let edgeProb True  True  = pProbBurstStickiness params
      edgeProb False False = pProbBGStickiness params
      edgeProb a     b     = 1 - edgeProb a (not b)
      bToDist True = expDist (pTauBurst params)
      bToDist False = expDist (pTauBG params)
      probBurst :: Bool -> Prob
      probBurst True = pProbBurst params
      probBurst False = 1 - pProbBurst params
      psi :: Bool -> Bool -> Time -> Prob
      psi b1 b2 tau2 = bToDist b2 tau2 * edgeProb b1 b2
      marginalize :: (Bool -> Prob) -> Prob
      marginalize f = sum $ map f [True, False]
      cliques = V.zip dts $ V.tail dts
      resolveB :: (a,a) -> Bool -> a
      resolveB (a,b) True  = a
      resolveB (a,b) False = b
      
      message delta1 (tau1,tau2) =
        let f b2 = marginalize $ \b1->resolveB delta1 b1 * psi b1 b2 tau2
        in (f True, f False)
      delta0 dts = message (1,1) (dts V.! 0, dts V.! 1)
      
      deltaFwd = V.scanl message (delta0 dts) $ V.tail cliques
      deltaBack = V.scanr (\(t1,t2) d2->message d2 (t2,t1))
                  (delta0 $ V.reverse dts) $ V.init cliques
      
      res = V.zipWith (\(aT,aF) (bT,bF)->(aT*bT + aF*bT, aT*bF + aF*bF)) deltaFwd deltaBack
  in V.map (\(a,b)->(a/(a+b), b/(a+b))) res

-- | The maximum likelihood setting for {B_i} given {P(B_i)}
inferBs' :: V.Vector BDist -> V.Vector Bool
inferBs' = V.map (\(bBurst,bBG)->bBurst>bBG)

-- | The maximum likelihood setting for {B_i} given {\tau_i}
inferBs :: Params -> V.Vector Time -> V.Vector Bool
inferBs params dts = inferBs' $ computeProbBs params dts

-- | `inferBernoulli (a,b) draws` is the maximum likelihood
-- parameter for a Bernoulli distribution under Beta prior given
-- `draws`
inferBernoulli :: (Double, Double) -> V.Vector Bool -> Prob
inferBernoulli (a,b) v = realToFrac $ (a + h) / (a + b + n)
  where h = realToFrac $ V.length $ V.filter id v
        n = realToFrac $ V.length v
  
-- | Infer the parameters from a setting of {B_i}
inferParams :: HyperParams -> Params -> V.Vector Bool -> Params
inferParams hypers params bs =
  params { --pProbBurst = inferBernoulli (0,0) bs
           pProbBurstStickiness = inferBernoulli (hpBurstStickiness hypers)
                                  $ V.map (uncurry (==))
                                  $ V.filter (\(a,b)->a) $ V.zip bs (V.tail bs)
         , pProbBGStickiness    = inferBernoulli (hpBGStickiness hypers)
                                  $ V.map (uncurry (==))
                                  $ V.filter (\(a,b)->not a) $ V.zip bs (V.tail bs)
         }
        
em :: V.Vector Time -> HyperParams -> Params -> Params
em dts hypers params =
  let pbs = computeProbBs params dts
  in inferParams hypers params $ inferBs' pbs
