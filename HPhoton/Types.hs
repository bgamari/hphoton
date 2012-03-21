{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module HPhoton.Types ( -- * Time
                       Time
                     , TimeDelta
                     , RealTime
                     , timeToRealTime
                     , realTimeToTime
                     , Span
                       -- * Time stamps
                     , Timestamps(Timestamps)
                     , tsFreq, tsStamps, tsJiffy
                     , duration, realDuration
                     ) where

import Data.Label
import Data.Word
import qualified Data.Vector.Unboxed as V
  
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Data.List (sort)

-- | A time in instrument-dependent ticks
type Time = Word64
-- | A difference between times in instrument-dependent ticks
type TimeDelta = Word64
                 
-- | A real time given in seconds
type RealTime = Double

-- | Convert a Time to a RealTime
timeToRealTime :: RealTime -> Time -> RealTime
timeToRealTime jiffy t = jiffy * realToFrac t

-- | Convert a RealTime to a Time
realTimeToTime :: RealTime -> RealTime -> Time
realTimeToTime jiffy rt = round $ rt / jiffy

-- | A span of time given by `(start,end)`
type Span = (Time, Time)

-- | Represents a monotonic series of timestamps
data Timestamps = Timestamps { _tsFreq :: Word64 -- ^ Ticks per second
                             , _tsStamps :: V.Vector Time -- ^ Timestamps in ticks
                             }
                  deriving (Show, Eq)
$(mkLabels [''Timestamps])

-- | Real time per tick
tsJiffy :: Timestamps :-> RealTime
tsJiffy = lens (\ts->1/realToFrac (get tsFreq ts))
               (\jiffy->set tsFreq (round $ 1/jiffy))

-- | The duration in ticks of a timestamp series
duration :: Timestamps -> Time
duration ts = V.last stamps - V.head stamps
  where stamps = get tsStamps ts

realDuration :: Timestamps -> RealTime
realDuration ts = timeToRealTime (get tsJiffy ts) $ duration ts

instance Arbitrary Timestamps where
  arbitrary = do
    NonEmpty ts <- arbitrary
    return $ Timestamps 1 (V.fromList $ sort $ map abs $ ts)
