
import Data.Vector.Unboxed as V
import Data.LogFloat

type ModelState = ModelState { msM :: V.Vector Bool
                             , msZ :: V.Vector Bool }


likelihood :: V.Vector Time -> ModelState -> LogFloat

update :: V.Vector Time -> ModelState -> ModelState

between :: Num a => a -> a -> a -> Bool
between a b x = x >= a && x <= b

probPoissonWindow :: ModelState -> V.Vector Time -> Int -> Time -> LogFloat
probPoissonWindow ms ts winSize t =
        let a = V.length $ V.filter (between (t-winSize) (t+winSize)) ts

