{-# LANGUAGE FlexibleInstances #-}

module HPhoton.Types ( -- * Time
                       Time
                     , TimeDelta
                     , RealTime
                     , Freq
                     , Span
                       -- * Values with discrete time
                     , Clocked(..)
                     , freq, jiffy
                     , unClocked
                     , withFreq, mapWithFreq
                     , withJiffy, mapWithJiffy
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

-- | A span of time given by `(start,end)`
type Span = (Time, Time)

-- | `Clocked freq a` is a value `a` annotated with a clock frequency `freq`
data Clocked a = Clocked Freq a deriving (Show, Eq)

instance Functor Clocked where
    fmap f (Clocked freq a) = Clocked freq (f a)

instance Foldable Clocked where
    foldMap f (Clocked _ a) = f a

instance Traversable Clocked where
    sequenceA (Clocked freq a) = fmap (Clocked freq) a

freq :: Clocked a -> Freq
freq (Clocked f _) = f

jiffy :: Clocked a -> RealTime
jiffy (Clocked freq _) = 1 / realToFrac freq

unClocked :: Clocked a -> a
unClocked (Clocked _ a) = a

mapWithFreq :: (Freq -> a -> b) -> Clocked a -> Clocked b
mapWithFreq f c = fmap (f $ freq c) c

withFreq :: (Freq -> a -> b) -> Clocked a -> b
withFreq f c = unClocked $ mapWithFreq f c
              
mapWithJiffy :: (RealTime -> a -> b) -> Clocked a -> Clocked b
mapWithJiffy f c = fmap (f $ jiffy c) c

withJiffy :: (RealTime -> a -> b) -> Clocked a -> b
withJiffy f c = unClocked $ mapWithJiffy f c
              
-- | The duration in ticks of a timestamp series
duration :: [V.Vector Time] -> Time
duration stamps = maximum (map V.last stamps) - minimum (map V.head stamps)

realDuration :: Clocked [V.Vector Time] -> RealTime
realDuration = timeToRealTime . fmap duration

-- | Convert a Time to a RealTime
timeToRealTime :: Clocked Time -> RealTime
timeToRealTime = withJiffy (\jiffy t->jiffy*realToFrac t)

-- | Convert a RealTime to a Time
realTimeToTime :: RealTime -> RealTime -> Clocked Time
realTimeToTime jiffy = realTimeToTime' (round $ 1/jiffy)

-- | Convert a RealTime to a Time
realTimeToTime' :: Freq -> RealTime -> Clocked Time
realTimeToTime' freq rt = Clocked freq $ round $ rt * realToFrac freq

instance Arbitrary (Clocked (V.Vector Time)) where
  arbitrary = do
    NonEmpty ts <- arbitrary
    return $ Clocked 1 (V.fromList $ sort $ map abs $ ts)

