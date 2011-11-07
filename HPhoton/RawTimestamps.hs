module HPhoton.RawTimestamps (readStamps) where

import HPhoton.Types
import Data.Vector.Storable.MMap

-- | Real 64-bit unsigned timestamps from file
readStamps :: FilePath -> IO (V.Vector Time)
readStamps fname = unsafeMMapVector fname Nothing

