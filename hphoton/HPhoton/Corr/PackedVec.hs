{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module HPhoton.Corr.PackedVec ( Time
                              , PackedVec (PVec)
                              , packedVec, packedVec'
                              , index
                              , shiftVec
                              , map
                              , dot
                              ) where

import           Control.Monad.ST
import           Data.Function               (on)
import qualified Data.Vector.Algorithms.Heap as VA
import qualified Data.Vector.Unboxed         as V
import           HPhoton.Types
import           Prelude                     hiding (map)

-- | An unboxed sparse vector
newtype PackedVec i v = PVec (V.Vector (i,v))
                      deriving (Show, Eq)

-- | Construct a PackedVec, ensuring that the entries are sorted.
packedVec :: (Ord i, V.Unbox i, V.Unbox v) => V.Vector (i,v) -> PackedVec i v
packedVec v = PVec $ runST $ do
                  v' <- V.thaw v
                  VA.sortBy (compare `on` fst) v'
                  V.freeze v'
{-# INLINEABLE packedVec #-}

-- | Construct a PackedVec assuming that the entries are already sorted.
packedVec' :: (V.Unbox i, V.Unbox v) => V.Vector (i,v) -> PackedVec i v
packedVec' = PVec

-- | Sparse vector dot product
dot :: (Ord i, Num v, V.Unbox i, V.Unbox v)
    => PackedVec i v -> PackedVec i v -> v
dot (PVec as) (PVec bs) = dot' as bs 0
{-# INLINEABLE dot #-}

dot' :: (Ord i, Eq i, Num v, V.Unbox i, V.Unbox v)
     => V.Vector (i,v) -> V.Vector (i,v) -> v -> v
dot' !as !bs !s
    | V.null as  = s
    | V.null bs  = s
    | aa == ab   = let x = snd a * snd b
                   in dot' (V.tail as) (V.tail bs) (s+x)
    | aa >  ab   = dot' as (V.tail bs) s
    | aa <  ab   = dot' (V.tail as) bs s
    where
    a = V.head as
    b = V.head bs
    aa = fst a
    ab = fst b

-- | Fetch element i
index :: (Eq i, Num v, V.Unbox i, V.Unbox v) => PackedVec i v -> i -> v
index (PVec v) i =
    case V.find (\(x,_)->x==i) v of
        Just (x,y) -> y
        Nothing    -> 0
{-# INLINEABLE index #-}

-- | Shift the abscissas in a sparse vector
shiftVec :: (Num i, V.Unbox i, V.Unbox v) => i -> PackedVec i v -> PackedVec i v
shiftVec shift (PVec v) = PVec $ V.map (\(a,o)->(a+shift, o)) v
{-# INLINEABLE shiftVec #-}

-- | Zero elements until index i
dropUntil :: (Ord i, V.Unbox i, V.Unbox v) => i -> PackedVec i v -> PackedVec i v
dropUntil i (PVec v) = PVec $ V.dropWhile (\(a,o)->a < i) v
{-# INLINEABLE dropUntil #-}

-- | Zero elements after index i
takeUntil :: (Ord i, V.Unbox i, V.Unbox v) => i -> PackedVec i v -> PackedVec i v
takeUntil i (PVec v) = PVec $ V.takeWhile (\(a,o)->a < i) v
{-# INLINEABLE takeUntil #-}

-- | Map operation
-- Note that this will only map non-zero entries
map :: (V.Unbox i, V.Unbox v, V.Unbox v') => (v -> v') -> PackedVec i v -> PackedVec i v'
map f (PVec v) = PVec $ V.map (\(x,y)->(x, f y)) v
{-# INLINEABLE map #-}

