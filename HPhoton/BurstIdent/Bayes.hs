{-# LANGUAGE PackageImports, TupleSections #-}

module HPhoton.BurstIdent.Bayes where

import Data.Vector.Unboxed ((!), Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as BS
import HPhoton.Types
import Data.List (foldl')

  
type PhotonIdx = Int
type Prob = Double

-- | LogP: Represents a log probability
data LogP a = LogP a deriving (Eq, Ord, Show)

instance (Real a, Floating a) => Num (LogP a) where
        LogP x + LogP y = let x' = min x y
                              y' = max x y
                          in  LogP (y' + log (1 + exp (x'-y')))
        LogP x * LogP y = LogP (x + y)
        abs (LogP x)    = LogP x
        signum (LogP x) = 1
        fromInteger x   = LogP (log (fromInteger x))

instance (Real a, Floating a) => Fractional (LogP a) where
        LogP x / LogP y = LogP (x - y)
        fromRational x  = LogP (log (fromRational x))

instance (Real a, Floating a) => Real (LogP a) where
        toRational (LogP x) = toRational $ exp x

-- | Encapsulates the parameters of the burst model
data ModelParams = ModelParams { mpWindow :: Int
                               , mpProbB :: Prob
                               , mpTauBurst :: Time
                               , mpTauBg :: Time }
                               deriving (Show)
                                        
mpProbNB = (1-) . mpProbB

-- | The PDF of an exponential distribution with inverse rate tau
exp_pdf :: Double -> Double -> Prob
exp_pdf tau t = 1/tau * exp(-t/tau)

prob_dt__b, prob_dt__nb :: ModelParams -> TimeDelta -> Prob
-- | P(dt | Burst)
prob_dt__b mp dt = 1/tau * exp(-realToFrac dt / tau) -- TODO: There should be a distribution over tau_burst?
                   where tau = realToFrac $ mpTauBurst mp
-- | P(dt | !Burst)
prob_dt__nb mp = exp_pdf (realToFrac $ mpTauBg mp) . realToFrac

-- | Compute the Bayes factor beta_n for a given photon i
beta :: ModelParams -> V.Vector TimeDelta -> PhotonIdx -> Double
beta mp dts i
        | i+n >= V.length dts = error "Out of range"
        | i-n < 0             = error "Out of range"
        | otherwise = let prob_b__dts = mpProbB mp * (product $ map (\j->prob_dt__b mp (dts!(i+j))) [-n..n])
                          prob_nb__dts = mpProbNB mp * (product $ map (\j->prob_dt__nb mp (dts!(i+j))) [-n..n])
                      in prob_b__dts / prob_nb__dts
        where n = mpWindow mp

-- | Compute beta for each photon in a stream
betas :: ModelParams -> V.Vector TimeDelta -> V.Vector (PhotonIdx, Double)
betas mp dts = V.map (\i->(i,beta mp dts i))
               $ V.enumFromN (fromIntegral n) (fromIntegral $ V.length dts - 2*n)
  where n = mpWindow mp
         
-- | Find photons attributable to a burst
findBurstPhotons :: ModelParams -> Double -> V.Vector TimeDelta -> V.Vector PhotonIdx
findBurstPhotons mp thresh dts = V.map fst
                                 $ V.filter (\(idx,beta)->beta > thresh)
                                 $ betas mp dts

data CompressSpansState = CSpansState { startT :: !Time
                                      , lastT  :: !Time
                                      , result :: V.Vector Span
                                      } deriving Show

-- | Reduce a list of times to a list of '(startTime, endTime)' spans
compressSpans :: Time -> V.Vector Time -> V.Vector Span
compressSpans fuzz ts =
  let f :: CompressSpansState -> Time -> CompressSpansState
      f s t  | t - lastT s <= fuzz  = s { lastT=t }
             | otherwise = s { startT = t
                             , lastT  = t
                             , result = result s V.++ V.singleton (startT s, lastT s)
                             }
      s = CSpansState { startT = V.head ts
                      , lastT  = V.head ts
                      , result = V.empty
                      }
      spans = result $ V.foldl f s ts
  in if V.null spans then V.empty
                     else spans 
                        
