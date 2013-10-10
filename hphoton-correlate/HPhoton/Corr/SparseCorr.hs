{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TupleSections #-}

module HPhoton.Corr.SparseCorr ( corr
                               , rebin
                               , Binned(..), binnedWidth, unBinned
                               , PackedVec
                               , BinnedVec
                               , vecFromStamps, unsafeVecFromStamps
                               ) where

import qualified Data.Vector.Generic as V
import Data.Foldable (foldl')
import Control.Monad

import HPhoton.Types       
import HPhoton.Corr.PackedVec (PackedVec(..))
import qualified HPhoton.Corr.PackedVec as PV
       
data Binned t a = Binned t a
                deriving (Show, Eq)
     
instance Functor (Binned t) where
    fmap f (Binned t a) = Binned t (f a)
     
binnedWidth :: Binned t a -> t
binnedWidth (Binned t _) = t
{-# INLINEABLE binnedWidth #-}

unBinned :: Binned t a -> a
unBinned (Binned _ a) = a
{-# INLINEABLE unBinned #-}

type BinnedVec v t a = Binned t (PackedVec v t a)

vecFromStamps :: (Num t, Ord t, V.Vector v t, V.Vector v (t,a), Num a)
              => v t -> Binned t (PackedVec v t a)
vecFromStamps = Binned 1 . PV.packedVec . V.map (,1) 
{-# INLINEABLE vecFromStamps #-}

unsafeVecFromStamps :: (Num t, Ord t, V.Vector v t, V.Vector v (t,a), Num a)
                    => v t -> Binned t (PackedVec v t a)
unsafeVecFromStamps = Binned 1 . PV.unsafePackedVec . V.map (,1) 
{-# INLINEABLE unsafeVecFromStamps #-}

-- | For condensing data into larger bins. This is sometimes desireable
-- when computing longer lag times.
rebin :: (Num t, Ord t, Integral t, V.Vector v (t,a), Num a, Eq a)
      => Int -> BinnedVec v t a -> BinnedVec v t a
rebin n v | n <= 0 = error "Invalid rebin size"
rebin 1 v = v
rebin n (Binned oldWidth v) = Binned width (PV.unsafePackedVec $ V.fromList bins)
    where width = oldWidth * fromIntegral n
          start_bin t = floor $ realToFrac t / realToFrac width
          bins = f (start_bin $ fst $ PV.head v) 0 $ V.toList $ getPackedVec v
          f bin accum [] = if accum /= 0 then [(bin*width, accum)]
                                         else []
          f bin accum ((a,o):rest) 
            | a <  width*bin      = error "Time went backwards. Did something overflow?"
            | a >= width*(bin+1)  = if accum /= 0
                                       then (bin*width, accum):f (start_bin a) o rest
                                       else f (start_bin a) o rest
            | otherwise           = f bin (accum+o) rest
{-# INLINEABLE rebin #-}

corr :: (Show t, Num t, Integral t, Ord t, Real a, V.Vector v (t,a))
     => t -> BinnedVec v t a -> BinnedVec v t a -> t -> (Double, Double)
corr longlag (Binned ta a) (Binned tb b) lag
    | ta /= tb           = error "Can't correlate vectors of different bin lengths"
    | lag < ta           = error $ "Lag must be larger than bin time"
    | lag `mod` ta /= 0  = error $ "Lag ("++show lag++") must be multiple of bin time of a ("++show ta++")"
corr longlag (Binned binWidth a) (Binned _ b) lag =
    let timespan x = (fst $ V.last x) - (fst $ V.head x)
        ta = timespan (getPackedVec a)
        tb = timespan (getPackedVec b)
        -- experiment length in bins
        t = fromIntegral (min ta tb) / realToFrac binWidth :: Double
        (sa,sb) = trimShiftData longlag a b lag
    
        dot = realToFrac $ PV.dot sa sb
        ss = realToFrac $ PV.dot (PV.map (^2) sa) (PV.map (^2) sb)
        count = realToFrac . V.foldl' (\a (_,b)->a+b) 0 . getPackedVec
        norm_denom = (count a / t) * (count b / t) :: Double
        g = dot / norm_denom / t
        bar2 = (ss / t - (dot / t)^2) / t / norm_denom^2
    in (g, sqrt bar2)
{-# INLINEABLE corr #-}

-- | Here we try to ensure that the zone is sized such that the same amount
-- of data is used in the correlation over various lags. This requires that
-- we know the longest lag for which we intend on calculating the correlation
-- function
--
-- We use the following scheme,
--
--  Legend:  ─ Ignored data
--           ═ Data used in correlation function
--
--  With a longlag of 5 character cells
--
--  Unshifted
--              𝑡=0    ↓ 𝑡=startT              ↓ 𝑡=endT
--    Channel A  |     ────════════════════════
--    Channel B  |     ────════════════════════
-- 
--  Shifted by 𝛥𝑡
--              𝑡=0
--    Channel A  |     ────════════════════════
--    Channel B  |         ════════════════════────
--  
trimShiftData
    :: (Ord t, Num t, Real a, V.Vector v (t,a))
    => t -> PackedVec v t a -> PackedVec v t a -> t -> (PackedVec v t a, PackedVec v t a)
trimShiftData longlag a b lag =
        let startT = max (fst $ PV.head a) (fst $ PV.head b)
            endT = min (fst $ PV.last a) (fst $ PV.last b)
            a' = PV.takeWhileIdx (<= endT)
               $ PV.dropWhileIdx (<  (startT + longlag)) a
            b' = PV.takeWhileIdx (<= endT)
               $ PV.dropWhileIdx (<  (startT + longlag))
               $ PV.shiftVec lag b
        in (a', b')
{-# INLINE trimShiftData #-}
