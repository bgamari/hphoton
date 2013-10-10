{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TupleSections #-}

module HPhoton.Corr.SparseCorr ( corr
                               , rebin
                               , Binned(..), binnedWidth, unBinned
                               , PackedVec
                               , BinnedVec
                               , vecFromStamps, vecFromStamps'
                               ) where

import qualified Data.Vector.Unboxed as V
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
           
unBinned :: Binned t a -> a
unBinned (Binned _ a) = a         

type BinnedVec t v = Binned t (PackedVec t v)

vecFromStamps :: (Num t, Ord t, V.Unbox t, V.Unbox v, Num v)
              => V.Vector t -> Binned t (PackedVec t v)
vecFromStamps = Binned 1 . PV.packedVec . V.map (,1) 
{-# INLINEABLE vecFromStamps #-}

vecFromStamps' :: (Num t, Ord t, V.Unbox t, V.Unbox v, Num v)
              => V.Vector t -> Binned t (PackedVec t v)
vecFromStamps' = Binned 1 . PV.packedVec' . V.map (,1) 
{-# INLINEABLE vecFromStamps' #-}

-- | For condensing data into larger bins. This is sometimes desireable
-- when computing longer lag times.
rebin :: (Num t, Ord t, Integral t, V.Unbox t, V.Unbox v, Num v, Eq v)
      => Int -> BinnedVec t v -> BinnedVec t v
rebin n v | n <= 0 = error "Invalid rebin size"
rebin 1 v = v
rebin n (Binned oldWidth (PVec v)) = Binned width (PVec $ V.fromList bins)
    where width = oldWidth * fromIntegral n
          start_bin t = floor $ realToFrac t / realToFrac width
          bins = f (start_bin $ fst $ V.head v) 0 $ V.toList v
          f bin accum [] = if accum /= 0 then [(bin*width, accum)]
                                         else []
          f bin accum ((a,o):rest) 
            | a <  width*bin      = error "Time went backwards. Did something overflow?"
            | a >= width*(bin+1)  = if accum /= 0
                                       then (bin*width, accum):f (start_bin a) o rest
                                       else f (start_bin a) o rest
            | otherwise           = f bin (accum+o) rest
{-# INLINEABLE rebin #-}

corr :: (Show t, Num t, Integral t, Ord t, Real v, V.Unbox t, V.Unbox v)
     => t -> BinnedVec t v -> BinnedVec t v -> t -> (Double, Double)
corr longlag (Binned ta a) (Binned tb b) lag
    | ta /= tb           = error "Can't correlate vectors of different bin lengths"
    | lag < ta           = error $ "Lag must be larger than bin time"
    | lag `mod` ta /= 0  = error $ "Lag ("++show lag++") must be multiple of bin time of a ("++show ta++")"
corr longlag (Binned binWidth a) (Binned _ b) lag =
    let timespan x = (fst $ V.last x) - (fst $ V.head x)
        ta = timespan (getPV a)
        tb = timespan (getPV b)
        -- experiment length in bins
        t = fromIntegral (min ta tb) / realToFrac binWidth :: Double
        (sa,sb) = trimShiftData longlag a b lag
    
        dot = realToFrac $ PV.dot sa sb
        ss = realToFrac $ PV.dot (PV.map (^2) sa) (PV.map (^2) sb)
        count = realToFrac . V.sum . V.map snd . getPV
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
    :: (Ord t, Num t, Real v, V.Unbox v, V.Unbox t)
    => t -> PackedVec t v -> PackedVec t v -> t -> (PackedVec t v, PackedVec t v)
trimShiftData longlag a b lag =
        let startT = max (fst $ PV.head a) (fst $ PV.head b)
            endT = min (fst $ PV.last a) (fst $ PV.last b)
            a' = PV.takeWhileIdx (<= endT)
               $ PV.dropWhileIdx (<  (startT + longlag)) a
            b' = PV.takeWhileIdx (<= endT)
               $ PV.dropWhileIdx (<  (startT + longlag))
               $ PV.shiftVec lag b
        in (a', b')
{-# INLINEABLE trimShiftData #-}
