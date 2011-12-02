module HPhoton.Types ( Time
                     , RealTime
                     , timeToRealTime
                     , realTimeToTime
                     ) where

import Data.Word

type Time = Word64

type RealTime = Double

timeToRealTime :: RealTime -> Time -> RealTime
timeToRealTime jiffy t = jiffy * realToFrac t

realTimeToTime :: RealTime -> RealTime -> Time
realTimeToTime jiffy rt = round $ rt / jiffy

