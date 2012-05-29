{-# LANGUAGE FlexibleInstances #-}

module HPhoton.Types ( -- * Time
                       Time
                     , TimeDelta
                     , RealTime
                     , Freq
                     , Span
                       -- * Values with discrete time
                     , Clock
                     , clockFromFreq, clockFromJiffy
                     , freq, jiffy
                     , startTime, endTime
                     , duration, realDuration
                     , timeToRealTime
                     , realTimeToTime
                     ) where

import Data.Word
import Data.Foldable hiding (maximum, minimum)   
import Data.Traversable 
import qualified Data.Vector.Unboxed as V
  
import Test.QuickCheck
import Data.List (sort)

-- | A time in instrument-dependent ticks
type Time = Word64
-- | A difference between times in instrument-dependent ticks
type TimeDelta = Word64
                 
-- | A real time given in seconds
type RealTime = Double
-- | A frequency in Hertz
type Freq = Word64
-- | A rate (e.g. photon intensity) in Hz
type Rate = Double

-- | `Clock` denotes a clockrate
newtype Clock = Clock Freq deriving (Show, Eq)

-- | Construct a Clock from a frequency              
clockFromFreq :: Freq -> Clock
clockFromFreq freq = Clock freq

-- | Construct a Clock from a period
clockFromJiffy :: RealTime -> Clock
clockFromJiffy jiffy = Clock $ round $ 1/jiffy

-- | `freq clock` is the frequency of `clock`               
freq :: Clock -> Freq
freq (Clock f) = f

-- | `jiffy clock` is the period of `clock`
jiffy :: Clock -> RealTime
jiffy (Clock freq) = 1 / realToFrac freq

checkEmpty :: String -> [a] -> [a]
checkEmpty e [] = error $ e++": Empty list"
checkEmpty _ a  = a          

-- | Gets the start time of a set of timestamps           
startTime :: [V.Vector Time] -> Time
startTime [] = error "startTime: No start time of empty list of timeseries"        
startTime stamps = minimum $ checkEmpty "startTime" $ foldMap head stamps
  where head x | V.null x  = []
               | otherwise = [V.head x]
          
-- | Gets the end time of a set of timestamps           
endTime :: [V.Vector Time] -> Time
endTime [] = error "endTime: No end time of empty list of timeseries"        
endTime stamps = maximum $ checkEmpty "endTime" $ foldMap last stamps
  where last x | V.null x  = []
               | otherwise = [V.last x]

-- | The duration in ticks of a timestamp series
duration :: [V.Vector Time] -> Time
duration stamps = endTime stamps - startTime stamps

-- | The duration in real time of a timestamp series
realDuration :: Clock -> [V.Vector Time] -> RealTime
realDuration clk = timeToRealTime clk . duration

-- | Convert a Time to a RealTime
timeToRealTime :: Clock -> Time -> RealTime
timeToRealTime clk t = jiffy clk * realToFrac t

-- | Convert a RealTime to a Time
realTimeToTime :: Clock -> RealTime -> Time
realTimeToTime clk rt = round $ rt / jiffy clk

-- | Sorted timestamps
newtype Timestamps = Timestamps (V.Vector Time) deriving (Show)

instance Arbitrary Timestamps where
  arbitrary = do
    NonEmpty ts <- arbitrary
    return $ Timestamps (V.fromList $ sort $ map abs $ ts)

-- | A span of time given by `(start,end)`
type Span = (Time, Time)

