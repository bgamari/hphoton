{-# LANGUAGE BangPatterns #-}

module HPhoton.Bin ( -- * Temporal photon binning
                     binMany
                   , bin
                   , binL
                   , binWithBounds
                   , binWithBoundsL
                   , binRange
                   , binRangeL
                   , binRangeWithBounds
                   , binRangeWithBoundsL
                   -- * Internal
                   , bin'
                   ) where

import HPhoton.Types
import qualified Data.Vector.Unboxed as V

import           Control.Applicative
import           Data.Foldable as F
import           Data.Traversable as T

-- | 'bin width times' is a list of binned counts of bin width 'width'
bin :: Time -> V.Vector Time -> V.Vector Int
bin width ts = V.fromList $ binL width ts

binL :: Time -> V.Vector Time -> [Int]
binL width ts =
    bin' width (V.toList ts) Nothing (fromIntegral $ V.head ts `quot` width) 0

-- | 'binWithBounds width times' is a list of bin counts and start
-- times of bin width 'width'
binWithBounds :: Time -> V.Vector Time -> V.Vector (Time, Int)
binWithBounds width ts = V.fromList $ binWithBoundsL width ts

binWithBoundsL :: Time -> V.Vector Time -> [(Time, Int)]
binWithBoundsL width ts = zip times bins
    where bin0 = fromIntegral $ V.head ts `quot` width
          bins = bin' width (V.toList ts) Nothing bin0 0
          times = map (\i->fromIntegral (i+bin0)*width) [0..]

-- | 'binRange width (start,end) times' is a list of bin counts
-- of bin width 'width'. The bins will begin at time 'start' and end
-- at 'end'
binRange :: Time -> (Time, Time) -> V.Vector Time -> V.Vector Int
binRange width (start_t, end_t) ts = V.fromList $ binRangeL width (start_t, end_t) ts

binRangeL :: Time -> (Time, Time) -> V.Vector Time -> [Int]
binRangeL width (start_t, end_t) ts =
    bin' width (V.toList ts) (Just end_t) (fromIntegral $ start_t `quot` width) 0

binRangeWithBounds :: Time -> (Time, Time) -> V.Vector Time -> V.Vector (Time, Int)
binRangeWithBounds width (start_t, end_t) ts =
    V.fromList $ binRangeWithBoundsL width (start_t, end_t) ts

binRangeWithBoundsL :: Time -> (Time, Time) -> V.Vector Time -> [(Time, Int)]
binRangeWithBoundsL width (start_t, end_t) ts = zip times bins
    where bin0 = fromIntegral $ start_t `quot` width
          bins = bin' width (V.toList ts) (Just end_t) bin0 0
          times = map (\i->fromIntegral (i+bin0)*width) [0..]

bin' :: Time -> [Time] -> Maybe Time -> Int -> Int -> [Int]
bin' width (t:ts) end_t !bin_n !count
     -- Make sure the current time is greater than start_t
    | t < fromIntegral bin_n*width       = bin' width ts end_t bin_n count
     -- Make sure the current time isn't past end of the current bin
    | t >= fromIntegral (bin_n+1)*width  =
        let rest = bin' width (t:ts) end_t (bin_n+1) 0
        in count : rest
     -- The photon is in our bin, increment count
    | otherwise                          = bin' width ts end_t bin_n (count+1)

bin' width [] (Just end_t) !bin_n !count
      -- Current photon is past the end of our current bin
    | end_t > fromIntegral bin_n*width   =
        let rest = bin' width [] (Just end_t) (bin_n+1) 0
        in count : rest
      -- The end
    | otherwise                          = []

bin' _ [] Nothing _ _                    = []

-- | Bin simultaneous photon streams. Here we take the bin range to be
-- the intersection of all of the streams.
binMany :: (Traversable f, Applicative f) => Time -> f (V.Vector Time) -> [f Int]
binMany binWidth times =
    getZipList $ T.sequenceA $ pure (ZipList . binRangeL binWidth (start,end)) <*> times
    where start = F.maximum $ fmap V.head times
          end   = F.minimum $ fmap V.last times
