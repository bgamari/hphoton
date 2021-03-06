{-# LANGUAGE FlexibleContexts, FlexibleInstances, TupleSections #-}

module HPhoton.Corr.SparseCorr
    ( -- * Computing correlation functions
      corr
    , logCorr
    , Point(..)
      -- * Binning photon timeseries
    , Binned(..), binnedWidth, unBinned
    , vecFromStamps, unsafeVecFromStamps
    , rebin
    , BinnedVec
    , PackedVec
      -- * Internal
    , trimShiftData
    ) where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.Vector.Generic as V
import Data.Vector.Fusion.Stream.Monadic (Step(..), Stream(..))
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import Data.Vector.Fusion.Bundle.Size as Size

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
rebin n (Binned oldWidth v)
  | PV.null v = Binned width PV.empty
  | otherwise =
        Binned width
        $ PV.unsafePackedVec $ V.unstream $ flip B.fromStream Size.Unknown
        $ rebinStream width
        $ PV.stream v
  where
    width = oldWidth * fromIntegral n
    binStart width t = (t `div` width) * width

    rebinStream :: (Monad m, Ord t, Num t, Integral t, Num a)
                => t -> Stream m (t,a) -> Stream m (t,a)
    rebinStream width (Stream stepa sa0) =
        Stream step (ReBinStart sa0)
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
     => t                 -- ^ largest expected lag
     -> t                 -- ^ longest expected grain size
     -> BinnedVec v t a   -- ^ first vector
     -> BinnedVec v t a   -- ^ second (shifted) vector
     -> t                 -- ^ lag to compute
     -> Either String (Double, Double)
corr longLag _ (Binned ta a) (Binned tb b) lag
  | ta /= tb                = Left "Can't correlate vectors of different bin widths"
  | lag < ta                = Left "Lag must be larger than bin time"
  | lag > longLag           = Left "Lag must be less than longlag"
  | lag `mod` ta /= 0       = Left "Lag must be multiple of bin time"
corr longLag largeGrain (Binned binWidth a) (Binned _ b) lag =
    let (sa,sb,zone) = trimShiftData longLag largeGrain a b lag
        ta = timespan sa
        tb = timespan sb

        inBins :: Integral t => t -> Double
        inBins t = fromIntegral t / realToFrac binWidth

        -- experiment length in bins
        t = inBins zone :: Double

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

-- | @trimShiftData longLag a b lag@ computes the subsets of 'PackedVec's @a@ and @b@
-- for computing a correlation at @lag@ with the intention of computing lags no larger
-- that @longLag@.
--
-- Here we ensure that the we use the same amount
-- of data is used in the correlation over various lags. This requires that
-- we know the longest lag for which we intend on calculating the correlation
-- function.
--
-- We use the following scheme,
--
-- @
--
--  Legend:  ─ Ignored data
--           ═ Data used in correlation function
--
--  With longLag = 4 character cells
--
--  Unshifted
--              τ=0    ↓ 𝑡=startT               ↓ 𝑡=endT
--    Channel A  |     ────════════════════════
--    Channel B  |     ────════════════════════
--
--  Shifted by τ=2 cells
--              𝑡=0
--    Channel A  |     ────════════════════════
--    Channel B  |       ──════════════════════──
--
--  Shifted by τ=longLag=4 cells
--              𝑡=0
--    Channel A  |     ────════════════════════
--    Channel B  |         ════════════════════────
-- @
trimShiftData
    :: (Ord t, Integral t, Real a, V.Vector v (t,a))
    => t     -- ^ the longest lag we will be computing the correlation of
    -> t     -- ^ the largest grain size we will be computing the correlation with
    -> PackedVec v t a  -- ^ first timeseries
    -> PackedVec v t a  -- ^ second timeseries
    -> t     -- ^ the lag
    -> (PackedVec v t a, PackedVec v t a, t) -- ^ the trimmed and shifted timeseries and the zone size
trimShiftData longLag _         _ _ lag | lag > longLag =
    error "HPhoton.Corr.SparseCor.trimShiftData: lag > longLag"
trimShiftData longLag longGrain a b lag =
    let startT = max (PV.startIdx a) (PV.startIdx b) + longLag
        endT = min (PV.endIdx a) (PV.endIdx b)
        -- zone size must be divisible by the longest grain size
        endT' = startT + ((endT - startT) `div` longGrain) * longGrain
        checkNull err v
            | PV.null v = error $ "HPhoton.Corr.SparseCorr.trimShiftData: "++err
            | otherwise = v
        a' = checkNull "a empty"
            $ PV.takeWhileIdx (<= endT')
            $ PV.dropWhileIdx (<  startT)
            $ a
        b' = checkNull "b empty"
            $ PV.takeWhileIdx (<= endT')
            $ PV.dropWhileIdx (<  startT)
            $ PV.shiftVec lag b
    in (a', b', endT' - startT)
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

        lags = takeWhile (< maxLag)
               $ let go binWidth lag (resize:rest) =
                       (lag*binWidth) : go (binWidth * fromIntegral resize) ((lag+1) `div` fromIntegral resize) rest
                 in go 1 1 binResizes

        realMaxLag = last lags

        largestGrain = fromIntegral $ foldl' (*) 1 $ map snd $ zip lags binResizes

        f (binSz:rest) lag a b
          | lag' > maxLag     = []
          | otherwise         =
            let (gee, bar) = either (\err->error $ "logCorr: Something went wrong: "++err) id
                             $ corr realMaxLag largestGrain a b lag'
            in Point lag' gee bar
               : f rest nextLag (rebin binSz a) (rebin binSz b)
          where
            lag' = fromIntegral lag * binnedWidth a
            nextLag = (lag + 1) `div` fromIntegral binSz
    in f binResizes 1 a b
{-# INLINE logCorr #-}
