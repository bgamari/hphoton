module HPhoton.Utils ( zeroTime
                     , zeroTimes
                     , timesToInterarrivals
                     , combineChannels
                     , invertSpans
                     , spansPhotons
                     , photonsDuration
                     ) where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Algorithms.Merge (sort)
import Control.Monad.Trans.State.Strict
import HPhoton.Types
import Control.Monad.ST

-- | Find the earliest time of a set of photon arrival times
zeroTime :: [V.Vector Time] -> Time
zeroTime = minimum . map V.head

-- | Offset a set of arrival times such that the first photon arrives at t=0
zeroTimes :: [V.Vector Time] -> [V.Vector Time]
zeroTimes times = map (V.map (\t->t-zeroTime times)) times

-- | Map a list of arrival times to the corresponding interarrival times
timesToInterarrivals :: V.Vector Time -> V.Vector TimeDelta
timesToInterarrivals times = V.zipWith (-) (V.tail times) times

-- | Combine multiple timestamp channels
combineChannels :: [V.Vector Time] -> V.Vector Time
combineChannels chs =
  runST $ do stamps <- V.thaw $ V.concat chs
             sort stamps
             V.freeze stamps

invertSpans :: (Time,Time) -> [Span] -> [Span]
invertSpans (start,end) ((a,b):rest)
  | start >= a && b > end   = invertSpans (b, end) rest            
  | start >= a && b <= end  = invertSpans (start, end) rest
  | start < a               = (start, a) : invertSpans (b, end) rest
  | b > end                 = []
  | a == start              = invertSpans (b,end) rest
invertSpans _ [] = []

-- | 'spansPhotons ts spans' returns the photons in a set of spans
spansPhotons :: V.Vector Time -> [Span] -> [V.Vector Time]
spansPhotons ts spans = evalState (mapM f spans) ts
  where f :: Span -> State (V.Vector Time) (V.Vector Time)
        f (start,end) = do ts <- get
                           -- Note that we use snd $ V.span here instead
                           -- of V.dropWhile due to bug
                           -- http://trac.haskell.org/vector/ticket/78
                           let (a,b) = V.span (<end) $ snd $ V.span (<start) ts
                           put b
                           return a
        
-- | The duration in RealTime of a stream of photons
photonsDuration :: RealTime -> V.Vector Time -> RealTime
photonsDuration jiffy times = (realToFrac $ V.last times - V.head times) * jiffy

