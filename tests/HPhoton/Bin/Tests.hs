module HPhoton.Bin.Tests (tests) where

import HPhoton.Types
import HPhoton.Bin

import Data.Label
import qualified Data.Vector.Unboxed as V
  
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Test.QuickCheck

prop_binning_conserves_photons :: Positive Time -> Timestamps -> Property
prop_binning_conserves_photons (Positive width) times =
  printTestCase (show bins)
  $ V.foldl (+) 0 bins == V.length takenTimes
  where Timestamps ts = times
        takenTimes = V.takeWhile (< (V.last ts `quot` width) * width) ts
        bins = binTimes ts width
        
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
