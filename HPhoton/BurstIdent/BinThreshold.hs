module HPhoton.BurstIdent.BinThreshold ( BinThreshold (..)
                                       , findBursts
                                       ) where

import HPhoton.Types
import HPhoton.Bin
import qualified Data.Vector.Unboxed as V
import Statistics.Sample

-- | Bin thresholding criterion
data BinThreshold = AbsThresh Int          -- ^ Absolute counts per bin
                  | AbsMeanThresh Int      -- ^ Counts above mean
                  | MultMeanThresh Double  -- ^ Multiple of mean
                  | MultVarThresh Double   -- ^ Multiple of variance above mean
                  deriving (Show, Eq)

findBursts :: TimeDelta -> BinThreshold -> V.Vector Time -> V.Vector Span
findBursts width threshold ts =
  let bins = binWithBounds width ts
      counts = V.map (realToFrac . snd) bins
      thresh = case threshold of
                    AbsThresh n      -> n
                    AbsMeanThresh n  -> round (mean counts) + n
                    MultMeanThresh x -> round $ x * mean counts
                    MultVarThresh x  -> let (m,s) = meanVariance counts
                                        in round $ m + x * s
  in V.map (\(t,n)->(t, t+width))
     $ V.filter (\(t,n)->n > thresh) bins
