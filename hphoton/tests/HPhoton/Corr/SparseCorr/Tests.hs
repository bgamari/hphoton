{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HPhoton.Corr.SparseCorr.Tests (tests) where

import           Data.Function                        (on)
import qualified Data.Vector.Unboxed                  as V

import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property

import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit

import           HPhoton.Corr.PackedVec               (PackedVec (..))
import           HPhoton.Corr.PackedVec.Tests         ()
import           HPhoton.Corr.SparseCorr
import           HPhoton.Types

instance (Num i, Ord i, V.Unbox i, V.Unbox v, Arbitrary i, Arbitrary v) =>
         Arbitrary (Binned i (PackedVec i v)) where
    arbitrary = do Positive width <- arbitrary
                   PVec pv <- arbitrary
                   return $ Binned width $ PVec $ V.map (\(x,y)->(x*width,y)) pv

prop_rebin_counts_invar :: (Num i, Integral i, Ord i, V.Unbox i, V.Unbox v, Num v, Eq v, Show v, Show i)
                        => Positive Int -> BinnedVec i v -> Result
prop_rebin_counts_invar (Positive width) bv@(Binned _ (PVec v)) =
    let Binned _ (PVec v') = rebin width bv
        initial = V.sum (V.map snd v)
        final = V.sum (V.map snd v')
    in if initial == final then succeeded
                           else failed {reason="Initial="++show initial++" final="++show (final-initial)}

prop_rebin_monotonic :: (Num i, Integral i, Ord i, V.Unbox i, V.Unbox v, Num v, Eq v, Show i, Show v)
                        => Positive Int -> BinnedVec i v -> Result
prop_rebin_monotonic (Positive width) bv =
    let Binned _ (PVec v') = rebin width bv
        dts = V.zipWith ((-) `on` fst) (V.tail v') v'
    in case () of
           _ | V.length v' < 2  -> rejected
           _ | V.all (>0) dts   -> succeeded
           _ | otherwise        -> failed { reason=show dts }

rebin_test1 = assertEqual "Case 1" res (rebin 10 ts)
    where ts = vecFromStamps $ V.fromList [1,2,3, 1001, 1002, 1003] :: BinnedVec Int Int
          res = Binned 10 $ PVec $ V.fromList [(0,3), (1000,3)]

tests =
    [ testProperty "Total counts invariant on rebinning"
          (prop_rebin_counts_invar :: Positive Int -> BinnedVec Time Int -> Result)
    , testProperty "Bin times monotonic"
          (prop_rebin_monotonic :: Positive Int -> BinnedVec Time Int -> Result)
    , testCase "Rebinning test" rebin_test1
    ]
