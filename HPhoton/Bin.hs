{-# LANGUAGE BangPatterns #-}

module HPhoton.Bin ( binTimes
                   , binTimesWithTimes
                   , binTimesInRange
                   , binTimesInRangeWithTimes
                     -- * Tests
                   , tests
                   ) where

import Data.Label
import HPhoton.Types
import Control.Monad
import qualified Data.Vector.Unboxed as V
  
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Test.QuickCheck

binTimes :: V.Vector Time -> Time -> V.Vector Int
binTimes ts width =
        V.fromList $ binTimes' (V.toList ts) width Nothing (fromIntegral $ V.head ts `quot` width) 0
                
binTimesWithTimes :: V.Vector Time -> Time -> V.Vector (Time, Int)
binTimesWithTimes ts width = V.zip times bins
        where bin0 = fromIntegral $ V.head ts `quot` width
              bins = V.fromList $ binTimes' (V.toList ts) width Nothing bin0 0
              times = V.generate (V.length bins) (\i->fromIntegral i*width + V.head ts)

binTimesInRange :: V.Vector Time -> Time -> (Time, Time) -> V.Vector Int
binTimesInRange ts width (start_t, end_t) =
        V.fromList $ binTimes' (V.toList ts) width (Just end_t) (fromIntegral $ start_t `quot` width) 0

binTimesInRangeWithTimes :: V.Vector Time -> Time -> (Time, Time) -> V.Vector (Time, Int)
binTimesInRangeWithTimes ts width (start_t, end_t) = V.zip times bins
        where bin0 = fromIntegral $ start_t `quot` width
              bins = V.fromList $ binTimes' (V.toList ts) width (Just end_t) bin0 0
              times = V.generate (V.length bins) (\i->fromIntegral i*width + start_t)

binTimes' :: [Time] -> Time -> Maybe Time -> Int -> Int -> [Int]
binTimes' (t:ts) width end_t !bin_n !count
        -- Make sure the current time is greater than start_t
        | t < fromIntegral bin_n*width       = binTimes' ts width end_t bin_n count
        -- Make sure the current time isn't past end of the current bin
        | t >= fromIntegral (bin_n+1)*width  =
            let rest = binTimes' (t:ts) width end_t (bin_n+1) 0 
            in count : rest
        -- The photon is in our bin, increment count
        | otherwise             = binTimes' ts width end_t bin_n (count+1)

binTimes' [] width (Just end_t) !bin_n !count
        | end_t > fromIntegral bin_n*width   = let rest = binTimes' [] width (Just end_t) (bin_n+1) 0
                                               in count : rest
        | otherwise                          = []

binTimes' [] _ Nothing _ _ = []

testTimes :: V.Vector Time
testTimes = V.generate (10*1000*1000) (\i->fromIntegral i*5)

main :: IO ()
main = do
        let binned = binTimes testTimes 10
        forM_ (V.toList binned) print

prop_binning_conserves_photons :: Positive Time -> Timestamps -> Property
prop_binning_conserves_photons (Positive width) ts =
  printTestCase (show bins)
  $ V.foldl (+) 0 bins == V.length takenTimes
  where times = get tsStamps ts
        takenTimes = V.takeWhile (< (V.last times `quot` width) * width) times
        bins = binTimes times width
        
test_bins_have_correct_count :: Time -> Int -> Assertion
test_bins_have_correct_count dt count =
  assertBool "Bin with incorrect count" $ V.all (==count) bins
  where times = V.enumFromStepN 0 dt (count*10)
        bins = binTimes times width
        width = fromIntegral count * dt

tests = [ testProperty "binning conserves photons" prop_binning_conserves_photons
        , testCase "bins have correct count (1,1)" $ test_bins_have_correct_count 1 1
        , testCase "bins have correct count (2,1)" $ test_bins_have_correct_count 2 1
        , testCase "bins have correct count (200,10)" $ test_bins_have_correct_count 200 10
        ]
