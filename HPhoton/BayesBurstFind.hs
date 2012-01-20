{-# LANGUAGE PackageImports, TupleSections #-}

module HPhoton.BayesBurstFind where

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
data ModelParams = ModelParams { prob_b :: Prob
                               , tau_burst :: Time
                               , tau_bg :: Time }
                               deriving (Show)
                                        
prob_nb = (1-) . prob_b

-- | The PDF of an exponential distribution with inverse rate tau
exp_pdf :: Double -> Double -> Prob
exp_pdf tau t = 1/tau * exp(-t/tau)

prob_dt__b, prob_dt__nb :: ModelParams -> Time -> Prob
-- | P(dt | Burst)
prob_dt__b mp dt = 1/tau * exp(-realToFrac dt / tau) -- TODO: There should be a distribution over tau_burst?
                   where tau = realToFrac $ tau_burst mp
-- | P(dt | !Burst)
prob_dt__nb mp = exp_pdf (realToFrac $ tau_bg mp) . realToFrac

-- | Compute the Bayes factor beta_n for a given photon i
beta :: Int -> V.Vector Time -> ModelParams -> Int -> Double
beta n dts mp i
        | i+n >= V.length dts = error "Out of range"
        | i-n < 0             = error "Out of range"
        | otherwise = let prob_b__dts = prob_b mp * (product $ map (\j->prob_dt__b mp (dts!(i+j))) [-n..n])
                          prob_nb__dts = prob_nb mp * (product $ map (\j->prob_dt__nb mp (dts!(i+j))) [-n..n])
                      in prob_b__dts / prob_nb__dts

-- | Find photons attributable to a burst
findBurstPhotons :: Int -> V.Vector Time -> ModelParams -> [PhotonIdx]
findBurstPhotons n dts mp =
  let accept i = beta n dts mp i > 2
  in filter accept [0..(fromIntegral $ V.length dts - n)]

data CompressSpansState = CSpansState { startT :: Time
                                      , lastT  :: Time
                                      , result :: [(Time,Time)]
                                      } deriving Show

-- | Reduce a list of times to a list of '(startTime, endTime, count)' spans
compressSpans :: Time -> [Time] -> [(Time, Time)]
compressSpans fuzz ts =
  let f :: CompressSpansState -> Time -> CompressSpansState
      f s t  | t - lastT s <= fuzz  = s { lastT=t }
             | otherwise = s { startT=t
                             , lastT=t
                             , result=(startT s, lastT s):result s }
      s = CSpansState { startT= -1
                      , lastT= -1
                      , result=[] }
      spans = result $ foldl' f s ts
  in if null spans then []
                   else tail $ reverse spans 
