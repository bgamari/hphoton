{-# LANGUAGE FlexibleContexts, FlexibleInstances, TupleSections #-}

module HPhoton.Corr.SparseCorr ( corr
                               , rebin
                               , Binned(..), binnedWidth, unBinned
                               , PackedVec
                               , BinnedVec
                               , vecFromStamps, unsafeVecFromStamps
                               , Point(..)
                               , logCorr
                               ) where

import qualified Data.Vector.Generic as V
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad
import           Data.Vector.Fusion.Stream.Monadic (Step(..), Stream(..))
import           Data.Vector.Fusion.Stream.Size

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
              => v t -> Maybe (Binned t (PackedVec v t a))
vecFromStamps v = Binned 1 <$> PV.packedVec (V.map (,1) v)
{-# INLINEABLE vecFromStamps #-}

unsafeVecFromStamps :: (Num t, Ord t, V.Vector v t, V.Vector v (t,a), Num a)
                    => v t -> Maybe (Binned t (PackedVec v t a))
unsafeVecFromStamps v = Binned 1 <$> PV.unsafePackedVec (V.map (,1) v)
{-# INLINEABLE unsafeVecFromStamps #-}

data ReBinState s t a
    = ReBinStart s
    | ReBinHaveBin s t a
    | ReBinDone

-- | For condensing data into larger bins. This is sometimes desireable
-- when computing longer lag times.
rebin :: (Num t, Ord t, Integral t, V.Vector v (t,a), Num a, Eq a)
      => Int -> BinnedVec v t a -> BinnedVec v t a
rebin n v | n <= 0 = error "Invalid rebin size"
rebin 1 v = v
rebin n (Binned oldWidth v) =
    case PV.unsafePackedVec $ V.unstream
         $ rebinStream width
         $ V.stream $ getPackedVec v of
      Nothing -> error "rebinStream broken length invariant"
      Just v' -> Binned width v'
  where
    width = oldWidth * fromIntegral n
    binStart width t = (t `div` width) * width
    rebinStream :: (Monad m, Ord t, Num t, Integral t, Num a)
                => t -> Stream m (t,a) -> Stream m (t,a)
    rebinStream width (Stream stepa sa0 na) =
        Stream step (ReBinStart sa0) (toMax na)
      where
        step (ReBinStart sa) = do
          r <- stepa sa
          return $ case r of
            Yield (t,a) sa'  -> Skip (ReBinHaveBin sa' (binStart width t) a)
            Skip sa'         -> Skip (ReBinStart sa')
            Done             -> Done
        step (ReBinHaveBin sa t0 a0) = do
          r <- stepa sa
          return $ case r of
            Yield (t,a) sa'
              | t < t0        -> error "SparseCorr.rebin: Time went backwards"
              | t >= t0+width -> Yield (t0,a0) (ReBinHaveBin sa' (binStart width t) a)
              | otherwise     -> Skip (ReBinHaveBin sa' t0 (a0+a))
            Skip sa'          -> Skip (ReBinHaveBin sa' t0 a0)
            Done              -> Yield (t0,a0) ReBinDone
        step ReBinDone = return Done
        {-# INLINE [0] step #-}
    {-# INLINE [0] rebinStream #-}
{-# INLINE [1] rebin #-}

-- | Compute the value of the cross-correlation function between two vectors
corr :: (Num t, Integral t, Ord t, Real a, V.Vector v (t,a))
     => t                 -- ^ Largest expected lag
     -> BinnedVec v t a   -- ^ first vector
     -> BinnedVec v t a   -- ^ second (shifted) vector
     -> t                 -- ^ Lag to compute
     -> Either String (Double, Double)
corr longlag (Binned ta a) (Binned tb b) lag
  | ta /= tb                = Left "Can't correlate vectors of different bin widths"
  | lag < ta                = Left "Lag must be larger than bin time"
  | lag > longlag           = Left "Lag must be less than longlag"
  | lag `mod` ta /= 0       = Left "Lag must be multiple of bin time"
corr longlag (Binned binWidth a) (Binned _ b) lag =
    let (sa,sb) = trimShiftData longlag a b lag
        ta = timespan sa
        tb = timespan sb

        -- experiment length in bins
        t = fromIntegral (min ta tb) / realToFrac binWidth :: Double

        (dot,ss) = case PV.dotSqr sa sb of (a,b) -> (realToFrac a, realToFrac b)
        count = realToFrac . PV.sum
        norm_denom = (count sa / t) * (count sb / t) :: Double
        g = dot / norm_denom / t
        bar2 = (ss / t - (dot / t)^2) / t / norm_denom^2
    in Right (g, sqrt bar2)
  where
    timespan :: (Num t, V.Vector v (t,a)) => PackedVec v t a -> t
    timespan pv = case PV.extent pv of (a,b) -> b - a
{-# INLINE corr #-}

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
        let startT = max (PV.startIdx a) (PV.startIdx b)
            endT = min (PV.endIdx a) (PV.endIdx b)
            a' = fromMaybe (error "a empty")
               $   PV.takeWhileIdx (<= endT)
               <=< PV.dropWhileIdx (<  (startT + longlag))
               $   a
            b' = fromMaybe (error "b empty")
               $ PV.takeWhileIdx (<= endT)
               <=< PV.dropWhileIdx (<  (startT + longlag))
               $ PV.shiftVec lag b
        in (a', b')
{-# INLINE trimShiftData #-}

data Point t a = Point { ptLag  :: !t
                       , ptCorr :: !a
                       , ptVar  :: !a
                       }
               deriving (Show)

-- | Multi-tau correlation
logCorr :: (V.Vector v (t, Int), Integral t)
        => t                     -- ^ Maximum lag
        -> Int                   -- ^ Number of lags per octave
        -> BinnedVec v t Int     -- ^ First vector
        -> BinnedVec v t Int     -- ^ Second (shifted) vector
        -> [Point t Double]      -- ^ Correlation function samples
logCorr maxLag lagsPerOctave a b =
    let binResizes = replicate (2*lagsPerOctave) 1
                  ++ cycle (take lagsPerOctave $ 2:repeat 1)

        f (binSz:rest) lag a b
          | lag' > maxLag     = []
          | otherwise         =
            let (gee, bar) = either (\err->error $ "logCorr: Something went wrong: "++err) id
                             $ corr maxLag a b lag'
            in Point lag' gee bar
               : f rest ((lag+1) `div` fromIntegral binSz) (rebin binSz a) (rebin binSz b)
          where
            lag' = fromIntegral lag * binnedWidth a
    in f binResizes 1 a b
{-# INLINE logCorr #-}
