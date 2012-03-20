module HPhoton.BurstIdent.BinThreshold where

import HPhoton.Types
import HPhoton.Bin
import qualified Data.Vector.Unboxed as V

findBursts :: TimeDelta -> Int -> V.Vector Time -> V.Vector Span
findBursts width threshold ts =
  let as = V.filter (\(t,n)->n > threshold)
           $ binTimesWithTimes ts width
  in V.map (\(t,n)->(t, t+width)) as