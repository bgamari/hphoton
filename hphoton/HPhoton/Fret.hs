module HPhoton.Fret ( fretEfficiency
                    , proximityRatio
                    , shotNoiseEVar
                      -- * Corrections
                    , gammaFromFret
                    , gammaFromRates
                    , correctGamma
                    , crosstalkFactor
                    , correctCrosstalk
                      -- * Utilities
                    , flipFrets, unflipFrets
                    , module HPhoton.Fret.Types
                    ) where

import           Control.Applicative
import           Data.Traversable    (traverse)
import           HPhoton.Fret.Types

-- | `fretEfficiency gamma fret` is the gamma-corrected FRET efficiency
-- for acceptor/donor intensities `fret`
fretEfficiency :: Gamma -> Fret Double -> FretEff
fretEfficiency gamma x = fretA x / (fretA x + fretD x)

-- | `proximityRatio fret` is the proximity ratio for acceptor/donor
-- intensities `fret`
proximityRatio :: Fret Double -> ProxRatio
proximityRatio = fretEfficiency 1

-- | Correct a proximity ratio with the given gamma
correctGamma :: Gamma -> ProxRatio -> FretEff
correctGamma gamma e = gamma / (gamma + 1/e - 1)

-- | `gammaFromFret proxRatio fretEff` is the gamma such that
-- `proxRatio` is shifted to `fretEff`
gammaFromFret :: ProxRatio -> FretEff -> Gamma
gammaFromFret proxRatio fretEff = (1/fretEff - 1) / (1/proxRatio - 1)

-- | 'gammaFromRates crosstalk donorOnly donorAcceptor' is an estimate
-- of gamma from count rates of donor-only and donor-acceptor populations
gammaFromRates :: Crosstalk -> Fret Double -> Fret Double -> Gamma
gammaFromRates crosstalk donorOnly donorAcceptor =
    crosstalk - fretA deltaI / fretD deltaI
  where deltaI = (-) <$> donorAcceptor <*> donorOnly

-- | Crosstalk factor from donor-only rates
crosstalkFactor :: Fret Double -> Crosstalk
crosstalkFactor fret = fretA fret / fretD fret

-- | Correct rates for crosstalk
correctCrosstalk :: Crosstalk -> Fret Double -> Fret Double
correctCrosstalk crosstalk fret =
    fret { fretA=fretA fret - lk }
  where lk = crosstalk * fretD fret

-- | Turn a 'Fret' of lists into a list of 'Fret's
flipFrets :: Fret [a] -> [Fret a]
flipFrets (Fret a b) = getZipList $ traverse ZipList $ Fret a b

-- | Turn a list of 'Fret's into a 'Fret' of lists
unflipFrets :: [Fret a] -> Fret [a]
unflipFrets xs = Fret (map fretA xs) (map fretD xs)

-- | The variance of a shot-noise limited FRET peak
shotNoiseEVar :: Double -> FretEff -> Double
shotNoiseEVar nTotal e = e * (1-e) / nTotal
