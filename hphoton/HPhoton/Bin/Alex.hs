module HPhoton.Bin.Alex (alexBin) where

import           Control.Applicative
import           Data.Foldable as F
import           Data.Traversable as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import           HPhoton.Bin
import           HPhoton.Types
import           HPhoton.Fret.Alex

alexBin :: Time -> Alex (VU.Vector Time) -> [Alex Int]
alexBin binWidth times =
    getZipList $ T.sequenceA $ pure (ZipList . binRangeL binWidth (start,end)) <*> times
    where start = F.maximum $ fmap VU.head times
          end   = F.minimum $ fmap VU.last times