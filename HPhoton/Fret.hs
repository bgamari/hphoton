module HPhoton.Fret ( fretEff
                    , proxRatio
                    , module HPhoton.Fret.Types
                    ) where

import HPhoton.Fret.Types
  
fretEff :: Double -> Fret Double -> Double
fretEff gamma x = fretA x / (fretA x + fretD x)

proxRatio = fretEff 1
