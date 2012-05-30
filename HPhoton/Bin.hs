{-# LANGUAGE BangPatterns #-}

module HPhoton.Bin ( bin
                   , binWithBounds
                   , binRange
                   , binRangeWithBounds
                   ) where

import HPhoton.Types
import qualified Data.Vector.Unboxed as V

bin :: Time -> V.Vector Time -> V.Vector Int
bin width ts =
    V.fromList $ bin' width (V.toList ts) Nothing (fromIntegral $ V.head ts `quot` width) 0
                
binWithBounds :: Time -> V.Vector Time -> V.Vector (Time, Int)
binWithBounds width ts = V.zip times bins
    where bin0 = fromIntegral $ V.head ts `quot` width
          bins = V.fromList $ bin' width (V.toList ts) Nothing bin0 0
          times = V.generate (V.length bins) (\i->fromIntegral i*width + V.head ts)

binRange :: Time -> V.Vector Time -> (Time, Time) -> V.Vector Int
binRange width ts (start_t, end_t) =
    V.fromList $ bin' width (V.toList ts) (Just end_t) (fromIntegral $ start_t `quot` width) 0

binRangeWithBounds :: Time -> V.Vector Time -> (Time, Time) -> V.Vector (Time, Int)
binRangeWithBounds width ts (start_t, end_t) = V.zip times bins
    where bin0 = fromIntegral $ start_t `quot` width
          bins = V.fromList $ bin' width (V.toList ts) (Just end_t) bin0 0
          times = V.generate (V.length bins) (\i->fromIntegral i*width + start_t)

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

