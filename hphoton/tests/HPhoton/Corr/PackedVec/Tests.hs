module HPhoton.Corr.PackedVec.Tests (tests) where

import           Control.Monad
import           Data.Function                        (on)
import qualified Data.Vector.Unboxed                  as V

import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers

import           HPhoton.Corr.PackedVec

prop_shift_unshift_invar :: (Num i, V.Unbox i, V.Unbox v, Eq v, Eq i)
                         => i -> PackedVec i v -> Bool
prop_shift_unshift_invar shift xs =
    (shiftVec (-shift) $ shiftVec shift xs) == xs

prop_dot_shift_invar :: (Num i, Ord i, Num v, Eq v, V.Unbox i, V.Unbox v, Show v)
                     => i -> PackedVec i v -> PackedVec i v -> Property
prop_dot_shift_invar shift x y =
    let dot1 = dot x y
        dot2 = dot (shiftVec shift x) (shiftVec shift y)
    in printTestCase ("initial="++show dot1++" final="++show dot2) (dot1==dot2)

instance (Num i, Ord i, V.Unbox i, V.Unbox v, Arbitrary i, Arbitrary v) =>
         Arbitrary (PackedVec i v) where
    arbitrary = do dxs <- arbitrary
                   let xs = scanl1 (+) $ fmap getPositive $ filter (<2^25) dxs
                   ys <- arbitrary
                   return $ PVec $ V.fromList $ zip xs ys

prop_dot_mag :: (V.Unbox i, V.Unbox v, Ord i, Num v, Eq v, Show v)
             => PackedVec i v -> Property
prop_dot_mag x =
    let PVec xs = x
        mag1 = dot x x
        mag2 = V.sum $ V.map (^2) $ V.map snd xs
    in printTestCase ("initial="++show mag1++" final="++show mag2) (mag1==mag2)

tests =
    [ testProperty "Dot product magnitude"
                   (prop_dot_mag :: PackedVec Int Int -> Property)
    , testProperty "Dot product shift invariant"
                   (prop_dot_shift_invar :: Int -> PackedVec Int Int -> PackedVec Int Int -> Property)
    , testProperty "Shift/unshift is identity"
                   (prop_shift_unshift_invar :: Int -> PackedVec Int Int -> Bool)
    ]

