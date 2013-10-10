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

-- | Shifted sparse dot product
shiftedDot :: (Ord t, Num t, Num v, V.Unbox t, V.Unbox v)
           => t -> PackedVec t v -> PackedVec t v -> v
shiftedDot shift a b = PV.dot a (PV.shiftVec shift b)
{-# INLINEABLE shiftedDot #-}

-- | Shifted sparse squared dot product
shiftedDot2 :: (Ord t, Num t, Num v, V.Unbox t, V.Unbox v)
            => t -> PackedVec t v -> PackedVec t v -> v
shiftedDot2 shift a b =
    let sqr = PV.map (^2)
    in PV.dot (sqr a) (sqr $ PV.shiftVec shift b)
{-# INLINEABLE shiftedDot2 #-}

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

-- | Compute the correlation function G(lag). We don't do anything here to
-- ensure that the zone size is sane, etc. This is left to calling code
corr' :: (Ord t, Num t, Integral t, V.Unbox t, V.Unbox v, Real v, Show t)
      => t -> PackedVec t v -> PackedVec t v -> t -> (Double, Double)
corr' width (PVec a) (PVec b) lag
    | V.null a || V.null b = (0,0)
    | otherwise =
        let timespan x = (fst $ V.last x) - (fst $ V.head x)
            ta = timespan a
            tb = timespan b
            t = fromIntegral (min ta tb) / realToFrac width :: Double
        
            dot = realToFrac $ shiftedDot lag (PVec a) (PVec b)
            ss = realToFrac $ shiftedDot2 lag (PVec a) (PVec b)
            count = realToFrac . V.sum . V.map snd
            norm_denom = (count a / t) * (count b / t) :: Double
            g = dot / norm_denom / t
            bar2 = (ss / t - (dot / t)^2) / t / norm_denom^2
        in (g, sqrt bar2)    
{-# INLINEABLE corr' #-}

corr :: (Show t, Num t, Integral t, Ord t, Real v, V.Unbox t, V.Unbox v)
     => t -> BinnedVec t v -> BinnedVec t v -> t -> (Double, Double)
corr longlag (Binned ta a) (Binned tb b) lag
    | ta /= tb           = error "Can't correlate vectors of different bin lengths"
    | lag < ta           = error $ "Lag must be larger than bin time"
    | lag `mod` ta /= 0  = error $ "Lag ("++show lag++") must be multiple of bin time of a ("++show ta++")"
corr longlag (Binned t a) (Binned _ b) lag =
    let (a',b') = trimData longlag a b lag
    in corr' t a' b' lag
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
--              𝑡=0
--    Channel A  |     ────════════════════════
--    Channel B  |     ────════════════════════
-- 
--  Shifted by 𝛥𝑡
--              𝑡=0
--    Channel A  |     ────════════════════════
--    Channel B  |         ════════════════════────
--  
trimData :: (Ord t, Num t, Real v, V.Unbox v, V.Unbox t)
         => t -> PackedVec t v -> PackedVec t v -> t -> (PackedVec t v, PackedVec t v)
trimData longlag (PVec a) (PVec b) lag =
        let startT = max (fst $ V.head a) (fst $ V.head b)
            endT = min (fst $ V.last a) (fst $ V.last b)
            a' = V.dropWhile (\(a,o) -> a < (startT + longlag)) a
            b' = V.dropWhile (\(a,o) -> a < (startT + longlag - lag)) b
            b'' = V.takeWhile (\(a,o) -> a <= (endT - lag)) b'
        in (PVec a', PVec b'')
{-# INLINEABLE trimData #-}
