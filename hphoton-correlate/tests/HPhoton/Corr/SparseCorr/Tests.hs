{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HPhoton.Corr.SparseCorr.Tests (tests) where

import           Data.Function                        (on)
import qualified Data.Vector.Generic                  as V
import qualified Data.Vector.Unboxed                  as VU

import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property

import           Test.Framework.Providers.HUnit (testCase)
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
    => BinnedVec v i a -> Positive Int -> Result
prop_rebin_counts_invar bv@(Binned _ v) (Positive width) =
    if initial == final
      then succeeded
      else failed {reason="Initial="++show initial++" final="++show (final-initial)}
  where
    Binned _ v' = rebin width bv
    initial = PV.sum v
    final = PV.sum v'

prop_rebin_monotonic
    :: (Num i, Integral i, Ord i, V.Vector v (i,a), V.Vector v i, Num a, Eq a, Show (v i))
    => BinnedVec v i a -> Positive Int -> Result
prop_rebin_monotonic bv (Positive width) =
    let Binned _ pv' = rebin width bv
        v' = PV.toVector pv'
        dts = V.zipWith ((-) `on` fst) (V.tail v') v'
    in case () of
           _ | V.length v' < 2  -> rejected
           _ | V.all (>0) dts   -> succeeded
           _ | otherwise        -> failed { reason=show dts }

rebin_test1 = assertEqual "Case 1" res (rebin 10 ts)
    where ts = vecFromStamps $ VU.fromList [1,2,3, 1001, 1002, 1003]
               :: BinnedVec VU.Vector Int Int
          res = Binned 10 $ PV.unsafePackedVec $ VU.fromList [(0,3), (1000,3)]
               :: BinnedVec VU.Vector Int Int

-- | Disambiguate types
withBV :: (BinnedVec VU.Vector Int Int -> a) -> BinnedVec VU.Vector Int Int -> a
withBV = id

tests =
    [ testProperty "Total counts invariant on rebinning"
        (withBV prop_rebin_counts_invar)
    , testProperty "Bin times monotonic"
        (withBV prop_rebin_monotonic)
    , testCase "Rebinning test" rebin_test1
    ]
