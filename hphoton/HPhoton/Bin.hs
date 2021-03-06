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
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import           Control.DeepSeq
import           Control.Applicative
import           Data.Foldable as F
import           Data.Traversable as T

-- | 'bin width times' is a list of binned counts of bin width 'width'
bin :: Time -> VU.Vector Time -> VU.Vector Int
bin width ts = VU.fromList $ binL width ts

binL :: Time -> VU.Vector Time -> [Int]
binL width ts =
    bin' width (VU.toList ts) Nothing (fromIntegral $ VU.head ts `quot` width) 0

-- | 'binWithBounds width times' is a list of bin counts and start
-- times of bin width 'width'
binWithBounds :: Time -> VU.Vector Time -> VU.Vector (Time, Int)
binWithBounds width ts = VU.fromList $ binWithBoundsL width ts

binWithBoundsL :: Time -> VU.Vector Time -> [(Time, Int)]
binWithBoundsL width ts = zip times bins
    where bin0 = fromIntegral $ VU.head ts `quot` width
          bins = bin' width (VU.toList ts) Nothing bin0 0
          times = map (\i->fromIntegral (i+bin0)*width) [0..]

-- | 'binRange width (start,end) times' is a list of bin counts
-- of bin width 'width'. The bins will begin at time 'start' and end
-- at 'end'
binRange :: Time -> (Time, Time) -> VU.Vector Time -> VU.Vector Int
binRange width (start_t, end_t) ts = VU.fromList $ binRangeL width (start_t, end_t) ts

binRangeL :: Time -> (Time, Time) -> VU.Vector Time -> [Int]
binRangeL width (start_t, end_t) ts =
    bin' width (VU.toList ts) (Just end_t) (fromIntegral $ start_t `quot` width) 0

binRangeWithBounds :: Time -> (Time, Time) -> VU.Vector Time -> VU.Vector (Time, Int)
binRangeWithBounds width (start_t, end_t) ts =
    VU.fromList $ binRangeWithBoundsL width (start_t, end_t) ts

binRangeWithBoundsL :: Time -> (Time, Time) -> VU.Vector Time -> [(Time, Int)]
binRangeWithBoundsL width (start_t, end_t) ts = zip times bins
    where bin0 = fromIntegral $ start_t `quot` width
          bins = bin' width (VU.toList ts) (Just end_t) bin0 0
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
binMany :: (NFData (f Int), Traversable f, Applicative f)
        => Time -> f (VU.Vector Time) -> V.Vector (f Int)
binMany binWidth times =
    force $ V.fromList
    $ getZipList $ T.sequenceA $ pure (ZipList . binRangeL binWidth (start,end)) <*> times
    where start = F.maximum $ fmap VU.head times
          end   = F.minimum $ fmap VU.last times
