module HPhoton.Types ( Time
                     , TimeDelta
                     , RealTime
                     , timeToRealTime
                     , realTimeToTime
                     ) where

import Data.Word

-- | A time in instrument-dependent ticks
type Time = Word64
-- | A difference between times in instrument-dependent ticks
type TimeDelta = Word64

type RealTime = Double

timeToRealTime :: RealTime -> Time -> RealTime
timeToRealTime jiffy t = jiffy * realToFrac t

realTimeToTime :: RealTime -> RealTime -> Time
realTimeToTime jiffy rt = round $ rt / jiffy

