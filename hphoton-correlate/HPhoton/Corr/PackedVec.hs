{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module HPhoton.Corr.PackedVec
    ( -- * Types
      PackedVec
      -- * Construction
    , packedVec, unsafePackedVec
    , empty
      -- * Queries
    , null
    , index
    , dot, dotSqr
    , sum
    , startIdx, endIdx
    , extent
      -- * Conversion
    , stream
    , toVector
      -- * Manipulation
    , shiftVec
    , map
    , izipWith
    , dropWhileIdx
    , takeWhileIdx
    ) where

import           Control.Monad.ST
import           Data.Function               (on)
import qualified Data.Vector.Algorithms.Heap as VA
import qualified Data.Vector.Generic         as V
import           Data.Vector.Fusion.Stream.Monadic (Step(..), Stream(..))
import           Data.Vector.Fusion.Stream.Size
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Util as S
import           Prelude                     hiding (map, head, last, sum, null, length)

-- | A sparse vector
data PackedVec v i a = PVec { shift :: !i
                            , getPackedVec :: !(v (i,a))
                            , startPos :: !Int
                            , length  :: !Int
                            }
-- invariants:
--   * startPos is position within getPackedVec identifying the first non-zero element of the packed vector
--   * length is the number of packed vector elements after startPos to be considered

deriving instance (Show i, Show (v (i,a))) => Show (PackedVec v i a)
instance (V.Vector v (i,a), Num i, Eq i, Eq a) => Eq (PackedVec v i a) where
    a == b = stream a == stream b

-- | Construct a 'PackedVec' from 'Vector', ensuring that the entries are sorted.
packedVec :: (Num i, Ord i, V.Vector v (i,a)) => v (i,a) -> PackedVec v i a
packedVec v = unsafePackedVec $ runST $ do
                  v' <- V.thaw v
                  VA.sortBy (compare `on` fst) v'
                  V.freeze v'
{-# INLINE packedVec #-}

-- | Construct a 'PackedVec' from 'Vector' assuming that the entries are already sorted.
unsafePackedVec :: (Num i, V.Vector v (i,a)) => v (i,a) -> PackedVec v i a
unsafePackedVec v =
    PVec { shift = 0
         , getPackedVec = v
         , startPos = 0
         , length = V.length v
         }
{-# INLINE [1] unsafePackedVec #-}
{-# RULES "constructPackedVec" forall x. unsafePackedVec (toVector x) = x #-}

-- | Zip elements with matching indices with the given function
izipWith :: (Num i, Ord i, V.Vector v (i,a), V.Vector v (i,b), V.Vector v (i,c))
         => (i -> a -> b -> c)
         -> PackedVec v i a -> PackedVec v i b -> PackedVec v i c
izipWith f as bs =
    unsafePackedVec $ V.unstream $ izipStreamsWith f (stream as) (stream bs)
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

-- | The dot product of two 'PackedVec's
dot :: (Num i, Ord i, Eq i, Num a, V.Vector v (i,a))
    => PackedVec v i a -> PackedVec v i a -> a
dot as bs = sum $ izipWith (const (*)) as bs
{-# INLINE dot #-}

-- | Strict pair
data Pair a b = Pair !a !b

-- | Produce a 'Stream' of the non-zero elements in the vector
stream :: (Num i, V.Vector v (i,a)) => PackedVec v i a -> Stream S.Id (i,a)
stream = V.stream . toVector
{-# INLINE stream #-}

-- | Produce a vector of the index-value pairs of the non-zero elements of a 'PackedVec'
toVector :: (Num i, V.Vector v (i,a)) => PackedVec v i a -> v (i,a)
toVector (PVec shift as startPos length) =
    V.map (\(i,v) -> (i+shift, v)) $ V.take length $ V.drop startPos as
{-# INLINE toVector #-}

-- | The dot product and squared-dot product of two 'PackedVec's
dotSqr :: (Num i, Ord i, Eq i, Num a, V.Vector v (i,a))
       => PackedVec v i a -> PackedVec v i a -> (a,a)
dotSqr as bs =
    pairToTuple
    $ S.foldl' (\(Pair a b) (c,d) -> Pair (a+c) (b+d)) (Pair 0 0)
    $ S.map snd
    $ izipStreamsWith (\_ a b->(a*b, a^2 * b^2)) (stream as) (stream bs)
  where
    pairToTuple (Pair a b) = (a, b)
{-# INLINE dotSqr #-}

-- | Fetch value at index
index :: (Num i, Eq i, Ord i, Num a, V.Vector v (i,a))
      => PackedVec v i a -> i -> a
index pvec i =
    case S.find (\(x,_)->x==i) $ S.take 1 $ S.dropWhile (\(i',_) -> i' < i) $ stream pvec of
     Just (_,y) -> y
     Nothing    -> 0
{-# INLINE index #-}

-- | Shift the abscissas in a sparse vector
shiftVec :: (Num i, V.Vector v (i,a)) => i -> PackedVec v i a -> PackedVec v i a
shiftVec s pvec = pvec { shift = s + shift pvec }
{-# INLINE shiftVec #-}

-- We implement these explicit with 'Stream's to avoid duplicating the vector
takeWhileIdx :: (Num i, V.Vector v (i,a))
             => (i -> Bool) -> PackedVec v i a -> PackedVec v i a
takeWhileIdx f pvec = pvec { length = S.length (S.takeWhile (f . fst) (stream pvec)) }
{-# INLINE takeWhileIdx #-}

dropWhileIdx :: (Num i, V.Vector v (i,a))
             => (i -> Bool) -> PackedVec v i a -> PackedVec v i a
dropWhileIdx f pvec = pvec { startPos = startPos pvec + delta
                           , length = length pvec - delta
                           }
  where delta = S.length (S.takeWhile (f . fst) (stream pvec))
{-# INLINE dropWhileIdx #-}

-- | Check whether a 'PackedVec' is empty
null :: (Num i, V.Vector v (i,a))
     => PackedVec v i a -> Bool
null = S.null . stream
{-# INLINE null #-}

-- | An empty 'PackedVec'
empty :: (V.Vector v (i,a), Num i) => PackedVec v i a
empty = unsafePackedVec V.empty
{-# INLINE empty #-}

-- | Map operation
-- Note that this will only map non-zero entries
map :: (Num i, V.Vector v (i,a), V.Vector v (i,b))
    => (a -> b) -> PackedVec v i a -> PackedVec v i b
map f = unsafePackedVec . V.map (\(x,y)->(x, f y)) . toVector
{-# INLINE map #-}

-- | The index of the first non-zero element
startIdx :: (Num i, V.Vector v (i,a)) => PackedVec v i a -> i
startIdx pvec = idx $ getPackedVec pvec V.! startPos pvec
  where idx (i,_) = i + shift pvec
{-# INLINE startIdx #-}

-- | The index of the last non-zero element
endIdx :: (Num i, V.Vector v (i,a)) => PackedVec v i a -> i
endIdx pvec = idx $ getPackedVec pvec V.! (length pvec - 1)
  where idx (i,_) = i + shift pvec
{-# INLINE endIdx #-}

-- | The sum of the elements
sum :: (Num i, Num a, V.Vector v (i,a)) => PackedVec v i a -> a
sum = S.foldl' (\accum (_,a)->accum+a) 0 . stream
{-# INLINE sum #-}

-- | The range of indicies covered by the vector
extent :: (Num i, V.Vector v (i,a)) => PackedVec v i a -> (i,i)
extent p = (startIdx p, endIdx p)
{-# INLINE extent #-}

