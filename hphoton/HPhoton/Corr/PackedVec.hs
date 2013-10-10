{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module HPhoton.Corr.PackedVec ( Time
                              , PackedVec, getPackedVec
                              , packedVec, unsafePackedVec
                              , index
                              , shiftVec
                              , map
                              , dot
                              , izipWith
                              , dropWhileIdx
                              , takeWhileIdx
                              , head
                              , last
                              ) where

import           Control.Monad.ST
import           Data.Function               (on)
import qualified Data.Vector.Algorithms.Heap as VA
import qualified Data.Vector.Generic         as V
import           Data.Vector.Fusion.Stream.Monadic (Step(..), Stream(..))
import           Data.Vector.Fusion.Stream.Size
import qualified Data.Vector.Fusion.Stream as S
import           HPhoton.Types
import           Prelude                     hiding (map, head, last)

-- | An unboxed sparse vector
newtype PackedVec v i a = PVec {getPackedVec :: v (i,a)}

-- | Construct a PackedVec, ensuring that the entries are sorted.
packedVec :: (Ord i, V.Vector v (i,a)) => v (i,a) -> PackedVec v i a
packedVec v = PVec $ runST $ do
                  v' <- V.thaw v
                  VA.sortBy (compare `on` fst) v'
                  V.freeze v'
{-# INLINE packedVec #-}

-- | Construct a PackedVec assuming that the entries are already sorted.
unsafePackedVec :: (V.Vector v (i,a)) => v (i,a) -> PackedVec v i a
unsafePackedVec = PVec
{-# INLINE unsafePackedVec #-}

izipWith :: (Ord i, V.Vector v (i,a), V.Vector v (i,b), V.Vector v (i,c))
         => (i -> a -> b -> c)
         -> PackedVec v i a -> PackedVec v i b -> PackedVec v i c
izipWith f (PVec as) (PVec bs) =
    PVec $ V.unstream $ izipStreamsWith f (V.stream as) (V.stream bs)
{-# INLINE izipWith #-}

data ZipState sa sb i a b
    = ZipStart sa sb
    | ZipAdvanceL sa sb i a
    | ZipAdvanceR sa sb i b

izipStreamsWith
    :: (Monad m, Ord i)
    => (i -> a -> b -> c)
    -> Stream m (i,a) -> Stream m (i,b) -> Stream m (i,c)
izipStreamsWith f (Stream stepa sa0 na) (Stream stepb sb0 nb) =
    Stream step (ZipStart sa0 sb0) (smaller na nb)
  where
    step (ZipStart sa sb) = do
      r <- stepa sa
      return $ case r of 
        Yield (vi, va) sa' -> Skip (ZipAdvanceR sa' sb vi va)
        Skip sa'           -> Skip (ZipStart sa' sb)
        Done               -> Done
    step (ZipAdvanceR sa sb ia va) = do
      r <- stepb sb
      return $ case r of
        Yield (ib, vb) sb' -> go sa sb' ia va ib vb
        Skip sb'           -> Skip (ZipAdvanceR sa sb' ia va)
        Done               -> Done
    step (ZipAdvanceL sa sb ib vb) = do
      r <- stepa sa
      return $ case r of
        Yield (ia, va) sa' -> go sa' sb ia va ib vb
        Skip sa'           -> Skip (ZipAdvanceL sa' sb ib vb)
        Done               -> Done
    {-# INLINE [0] step #-}

    go sa sb ia va ib vb =
      case compare ia ib of
        LT   -> Skip (ZipAdvanceL sa sb ib vb)
        EQ   -> Yield (ia, f ia va vb) (ZipStart sa sb)
        GT   -> Skip (ZipAdvanceR sa sb ia va)
    {-# INLINE [0] go #-}
{-# INLINE [1] izipStreamsWith #-}

dotStream' :: (Ord i, Eq i, Num a, V.Vector v (i,a))
     => v (i,a) -> v (i,a) -> a
dotStream' as bs =
    S.foldl' (+) 0 $ S.map snd $ izipStreamsWith (const (*)) (V.stream as) (V.stream bs)
{-# INLINE dotStream' #-}

-- | Sparse vector dot product
dot :: (Ord i, Num a, V.Vector v (i,a))
    => PackedVec v i a -> PackedVec v i a -> a
dot (PVec as) (PVec bs) = dotStream' as bs
{-# INLINE dot #-}

-- | Fetch element i
index :: (Eq i, Num a, V.Vector v (i,a)) => PackedVec v i a -> i -> a
index (PVec v) i =
    case V.find (\(x,_)->x==i) v of
        Just (x,y) -> y
        Nothing    -> 0
{-# INLINE index #-}

-- | Shift the abscissas in a sparse vector
shiftVec :: (Num i, V.Vector v (i,a)) => i -> PackedVec v i a -> PackedVec v i a
shiftVec shift (PVec v) = PVec $ V.map (\(a,o)->(a+shift, o)) v
{-# INLINE shiftVec #-}

takeWhileIdx :: (Ord i, V.Vector v (i,a))
             => (i -> Bool) -> PackedVec v i a -> PackedVec v i a
takeWhileIdx f (PVec v) = PVec $ V.takeWhile (f . fst) v
{-# INLINE takeWhileIdx #-}

dropWhileIdx :: (Ord i, V.Vector v (i,a))
             => (i -> Bool) -> PackedVec v i a -> PackedVec v i a
dropWhileIdx f (PVec v) = PVec $ V.dropWhile (f . fst) v
{-# INLINE dropWhileIdx #-}

-- | Map operation
-- Note that this will only map non-zero entries
map :: (V.Vector v (i,a), V.Vector v (i,b))
    => (a -> b) -> PackedVec v i a -> PackedVec v i b 
map f (PVec v) = PVec $ V.map (\(x,y)->(x, f y)) v
{-# INLINE map #-}

head :: (Ord i, V.Vector v (i,a)) => PackedVec v i a -> (i,a)
head (PVec v) = V.head v
{-# INLINE head #-}

last :: (Ord i, V.Vector v (i,a)) => PackedVec v i a -> (i,a)
last (PVec v) = V.last v
{-# INLINE last #-}
