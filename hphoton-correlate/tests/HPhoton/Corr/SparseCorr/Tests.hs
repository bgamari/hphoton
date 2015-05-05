{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HPhoton.Corr.SparseCorr.Tests (tests) where

import           Data.Function                        (on)
import qualified Data.Vector.Generic                  as V
import qualified Data.Vector.Unboxed                  as VU

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property (Result, succeeded, failed, rejected)

import           Test.Tasty.HUnit (testCase)
import           Test.HUnit

import           HPhoton.Corr.PackedVec               (PackedVec (..))
import qualified HPhoton.Corr.PackedVec               as PV
import           HPhoton.Corr.PackedVec.Tests         ()
import           HPhoton.Corr.SparseCorr
import           HPhoton.Types

instance (Num i, Ord i, V.Vector v a, V.Vector v (i,a), Arbitrary i, Arbitrary a) =>
         Arbitrary (Binned i (PackedVec v i a)) where
    arbitrary = do Positive width <- arbitrary
                   pv <- arbitrary
                   return $ Binned width $ PV.unsafePackedVec
                          $ V.map (\(x,y)->(x*width,y)) $ PV.toVector pv

prop_rebin_counts_invar
    :: (Num i, Integral i, Ord i, V.Vector v (i,a), Num a, Eq a, Show a, Show i)
    => BinnedVec v i a -> Positive Int -> Property
prop_rebin_counts_invar bv@(Binned _ v) (Positive width) =
    counterexample ("Initial="++show initial++" final="++show (final-initial)) (initial == final)
  where
    Binned _ v' = rebin width bv
    initial = PV.sum v
    final = PV.sum v'

prop_rebin_monotonic
    :: (Num i, Integral i, Ord i, V.Vector v (i,a), V.Vector v i, Num a, Eq a, Show (v i))
    => BinnedVec v i a -> Positive Int -> Property
prop_rebin_monotonic bv (Positive width) =
    let Binned _ pv' = rebin width bv
        v' = PV.toVector pv'
        dts = V.zipWith ((-) `on` fst) (V.tail v') v'
    in counterexample (show dts) $ (V.length v' < 2) ==> V.all (>0) dts

rebin_test1 = assertEqual "Case 1" res (rebin 10 ts)
    where ts = vecFromStamps $ VU.fromList [1,2,3, 1001, 1002, 1003]
               :: BinnedVec VU.Vector Int Int
          res = Binned 10 $ PV.unsafePackedVec $ VU.fromList [(0,3), (1000,3)]
               :: BinnedVec VU.Vector Int Int

trimShiftData_test1 :: TestTree
trimShiftData_test1 =
    testGroup "trimShiftData" $ map testLag [0, 10, maxLag]
  where
    testLag :: Int -> TestTree
    testLag lag =
        testCase ("lag="++show lag) $ do
            assertEqual "Case 1" resA (PV.toVector testA)
            assertEqual "Case 2" (resB lag) (PV.toVector testB)

      where (testA, testB) = trimShiftData maxLag 10 xs xs lag

    maxLag = 100
    xs :: PV.PackedVec VU.Vector Int Int
    xs = PV.packedVec $ V.fromList $ map (\t->(t,1)) [0,10..1000]

    resA = PV.toVector $ PV.packedVec $ V.fromList $ map (\t->(t,1)) [maxLag,10+maxLag..1000]
    resB lag = PV.toVector $ PV.packedVec $ V.fromList $ map (\t->(t,1)) [maxLag,maxLag+10..1000]

-- | Disambiguate types
withBV :: (BinnedVec VU.Vector Int Int -> a) -> BinnedVec VU.Vector Int Int -> a
withBV = id

tests = testGroup "SparseCorr"
    [ testProperty "Total counts invariant on rebinning"
        (withBV prop_rebin_counts_invar)
    , testProperty "Bin times monotonic"
        (withBV prop_rebin_monotonic)
    , testCase "Rebinning test" rebin_test1
    , trimShiftData_test1
    ]
