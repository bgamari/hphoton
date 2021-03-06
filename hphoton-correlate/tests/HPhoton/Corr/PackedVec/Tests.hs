{-# LANGUAGE UndecidableInstances, FlexibleContexts, FlexibleInstances #-}

module HPhoton.Corr.PackedVec.Tests (tests) where

import           Control.Monad
import           Data.Function                        (on)
import qualified Data.Vector.Generic                  as V
import qualified Data.Vector.Unboxed                  as VU

import           Test.Tasty
import           Test.Tasty.QuickCheck
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

prop_dotSqr_shift_invar :: (Num i, Ord i, Num a, Eq a, V.Vector v (i,a), Show a)
                        => PackedVec v i a -> PackedVec v i a -> i -> Property
prop_dotSqr_shift_invar x y shift =
    let dot1 = PV.dotSqr x y
        dot2 = PV.dotSqr (shiftVec shift x) (shiftVec shift y)
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

prop_dropWhileIdx :: ( V.Vector v a, V.Vector v (i,a)
                     , Num a, Eq a, Show a
                     , Show i, Num i, Ord i
                     , Show (v (i,a)), Eq (v (i,a)))
                  => PackedVec v i a -> Blind (i -> Bool) -> Property
prop_dropWhileIdx xs (Blind f) =
    let a = PV.unsafePackedVec $ V.dropWhile (\(i,_) -> f i) $ PV.toVector xs
        b = PV.dropWhileIdx f xs
    in counterexample ("good="++show a++" bad="++show b) (a == b)

prop_takeWhileIdx :: ( V.Vector v a, V.Vector v (i,a)
                     , Num a, Eq a, Show a
                     , Show i, Num i, Ord i
                     , Show (v (i,a)), Eq (v (i,a)))
                  => PackedVec v i a -> Blind (i -> Bool) -> Property
prop_takeWhileIdx xs (Blind f) =
    let a = PV.unsafePackedVec $ V.takeWhile (\(i,_) -> f i) $ PV.toVector xs
        b = PV.takeWhileIdx f xs
    in counterexample ("good="++show a++" bad="++show b) (a == b)

prop_dense_dot_mag :: (V.Vector v a, V.Vector v (Int,a), Num a, Eq a, Show a)
                   => v a -> v a -> Property
prop_dense_dot_mag xs ys =
    let mag1 = dot (unsafePackedVec $ V.indexed xs) (unsafePackedVec $ V.indexed ys)
        mag2 = V.sum $ V.zipWith (*) xs ys
    in counterexample ("initial="++show mag1++" final="++show mag2) (mag1==mag2)

prop_dense_dotSqr_mag :: (V.Vector v a, V.Vector v (Int,a), Num a, Eq a, Show a)
                      => v a -> v a -> Property
prop_dense_dotSqr_mag xs ys =
    let mag1 = dotSqr (unsafePackedVec $ V.indexed xs) (unsafePackedVec $ V.indexed ys)
        ss2 = V.sum $ V.zipWith (*) (V.map (^2) xs) (V.map (^2) ys)
        dot2 = V.sum $ V.zipWith (*) xs ys
    in counterexample ("initial="++show mag1++" final="++show (dot2,ss2)) (mag1 == (dot2,ss2))

instance (VU.Unbox a, Arbitrary a) => Arbitrary (VU.Vector a) where
    arbitrary = sized $ \n->V.replicateM n arbitrary

-- | Disambiguate types
withPV :: (PackedVec VU.Vector Int Int -> a) -> PackedVec VU.Vector Int Int -> a
withPV = id

tests = testGroup "PackedVec"
    [ testProperty "Dot product magnitude" (withPV prop_dot_mag)
    , testProperty "Dense dot product magnitude"
                   (prop_dense_dot_mag :: VU.Vector Int -> VU.Vector Int -> Property)
    , testProperty "Dense dot, squared dot product magnitude"
                   (prop_dense_dotSqr_mag :: VU.Vector Int -> VU.Vector Int -> Property)
    , testProperty "Dot product shift invariant" (withPV prop_dot_shift_invar)
    , testProperty "Dot, squared dot product shift invariant"
      (withPV prop_dotSqr_shift_invar)
    , testProperty "Shift/unshift is identity" (withPV prop_shift_unshift_invar)
    , testProperty "dropWhileIdx works" (withPV prop_dropWhileIdx)
    , testProperty "takeWhileIdx works" (withPV prop_takeWhileIdx)
    ]

