{-# LANGUAGE UndecidableInstances, FlexibleContexts, FlexibleInstances #-}

module HPhoton.Corr.PackedVec.Tests (tests) where

import           Control.Monad
import           Data.Function                        (on)
import qualified Data.Vector.Generic                  as V
import qualified Data.Vector.Unboxed                  as VU

import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers

import           HPhoton.Corr.PackedVec as PV

prop_shift_unshift_invar :: (Num i, V.Vector v (i,a), Eq (PackedVec v i a))
                         => PackedVec v i a -> i -> Bool
prop_shift_unshift_invar xs shift =
    shiftVec (-shift) (shiftVec shift xs) == xs

prop_dot_shift_invar :: (Num i, Ord i, Num a, Eq a, V.Vector v (i,a), Show a)
                     => PackedVec v i a -> PackedVec v i a -> i -> Property
prop_dot_shift_invar x y shift =
    let dot1 = dot x y
        dot2 = dot (shiftVec shift x) (shiftVec shift y)
    in counterexample ("initial="++show dot1++" final="++show dot2) (dot1==dot2)

instance (Num i, Ord i, Arbitrary i, Arbitrary a, V.Vector v (i,a)) =>
         Arbitrary (PackedVec v i a) where
    arbitrary = do dxs <- arbitrary
                   let xs = scanl1 (+) $ filter (<2^25) $ fmap getPositive dxs
                   ys <- arbitrary
                   return $ unsafePackedVec $ V.fromList $ zip xs ys

prop_dot_mag :: (V.Vector v a, V.Vector v (i,a), Num i, Ord i, Num a, Eq a, Show a)
             => PackedVec v i a -> Property
prop_dot_mag x =
    let mag1 = dot x x
        mag2 = V.sum $ V.map (^2) $ V.map snd $ PV.toVector x
    in counterexample ("initial="++show mag1++" final="++show mag2) (mag1==mag2)

prop_dense_dot_mag :: (V.Vector v a, V.Vector v (Int,a), Num a, Eq a, Show a)
                   => v a -> v a -> Property
prop_dense_dot_mag xs ys =
    let mag1 = dot (unsafePackedVec $ V.indexed xs) (unsafePackedVec $ V.indexed ys)
        mag2 = V.sum $ V.zipWith (*) xs ys
    in counterexample ("initial="++show mag1++" final="++show mag2) (mag1==mag2)

instance (VU.Unbox a, Arbitrary a) => Arbitrary (VU.Vector a) where
    arbitrary = sized $ \n->V.replicateM n arbitrary

-- | Disambiguate types
withPV :: (PackedVec VU.Vector Int Int -> a) -> PackedVec VU.Vector Int Int -> a
withPV = id

tests =
    [ testProperty "Dot product magnitude" (withPV prop_dot_mag)
    , testProperty "Dense dot product magnitude"
                   (prop_dense_dot_mag :: VU.Vector Int -> VU.Vector Int -> Property)
    , testProperty "Dot product shift invariant" (withPV prop_dot_shift_invar)
    , testProperty "Shift/unshift is identity" (withPV prop_shift_unshift_invar)
    ]

