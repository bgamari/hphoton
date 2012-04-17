module HPhoton.BurstIdent.BinThreshold ( BinThreshold (..)
                                       , findBursts
                                       ) where

import HPhoton.Types
import HPhoton.Bin
import qualified Data.Vector.Unboxed as V
import Statistics.Distribution
import Statistics.Distribution.Normal       

-- | Bin thresholding criterion
data BinThreshold = AbsThresh Int     -- ^ Absolute counts per bin
                  | MeanThresh Double -- ^ Multiple of mean
                  | VarThresh Double  -- ^ Multiple of variance above mean
                  deriving (Show, Eq)

findBursts :: TimeDelta -> BinThreshold -> V.Vector Time -> V.Vector Span
findBursts width threshold ts =
  let bins = binTimesWithTimes ts width
      countDist = normalFromSample $ V.map (realToFrac . snd) bins
      thresh = case threshold of
                    AbsThresh n  -> n
                    MeanThresh x -> round $ x * mean countDist
                    VarThresh x  -> round $ mean countDist + x * variance countDist
  in V.map (\(t,n)->(t, t+width))
     $ V.filter (\(t,n)->n > thresh) bins
