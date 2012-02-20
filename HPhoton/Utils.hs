module HPhoton.Utils ( zeroTime
                     , zeroTimes
                     , timesToInterarrivals
                     , combineChannels
                     ) where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Algorithms.Merge (sort)
import HPhoton.Types

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
combineChannels :: [V.Vector Time] -> IO (V.Vector Time)
combineChannels chs = do stamps <- V.thaw $ V.concat chs
                         sort stamps
                         stamps' <- V.freeze stamps
                         return stamps'
                         --return $ V.map (- V.head stamps) stamps
