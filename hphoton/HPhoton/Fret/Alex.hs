{-# LANGUAGE DeriveGeneric #-}

module HPhoton.Fret.Alex ( DirFactor
                         , Alex(..)
                         , proxRatio
                         , fretEff
                         , stoiciometry
                         , stoiciometry'
                           -- * Corrections
                         , directAExc
                         , correctDirectAExc
                         , crosstalkFactor
                         , correctCrosstalk
                         , gammaFromRates
                           -- * Reexports
                         , module HPhoton.Fret.Types
                         , module HPhoton.Fret
                         ) where

import           Control.Applicative
import           Data.Traversable
import           Data.Foldable
import           Data.Monoid

import qualified Data.Binary as B
import           GHC.Generics (Generic)

import           HPhoton.Fret.Types (FretEff, ProxRatio, Gamma, Crosstalk)
import           HPhoton.Fret (correctGamma, shotNoiseEVar)

-- | Direct acceptor excitation factor /d/
type DirFactor = Double

data Alex a = Alex { alexAexcAem :: !a
                   , alexAexcDem :: !a
                   , alexDexcAem :: !a
                   , alexDexcDem :: !a
                   }
            deriving (Show, Eq, Generic)

instance (B.Binary a) => B.Binary (Alex a)

instance Functor Alex where
  fmap f (Alex a b c d) = Alex (f a) (f b) (f c) (f d)

instance Applicative Alex where
  pure x = Alex x x x x
  (Alex a b c d) <*> (Alex x y z w) = Alex (a x) (b y) (c z) (d w)

instance Foldable Alex where
  foldMap f (Alex a b c d) = f a <> f b <> f c <> f d

instance Traversable Alex where
  traverse f (Alex a b c d) = Alex <$> f a <*> f b <*> f c <*> f d

instance Monoid a => Monoid (Alex a) where
  mempty = pure mempty
  a `mappend` b = mappend <$> a <*> b

fretEff :: Double -> Alex Double -> Double
fretEff gamma alex = iA / (iA + gamma*iD)
  where iA = alexDexcAem alex
        iD = alexDexcDem alex

proxRatio = fretEff 1

-- | Stoiciometry
stoiciometry :: Alex Double -> Double
stoiciometry = stoiciometry' 1

-- | Stoiciometry with Gamma
stoiciometry' :: Double -> Alex Double -> Double
stoiciometry' gamma alex =
    (gamma*iDexcDem + iDexcAem) / (gamma*iDexcDem + iDexcAem + iAexcDem + iAexcAem)
  where Alex { alexAexcAem = iAexcAem
             , alexAexcDem = iAexcDem
             , alexDexcDem = iDexcDem
             , alexDexcAem = iDexcAem
             } = alex

-- | 'Dir' acceptor direct excitation factor
directAExc :: Alex Double -> DirFactor
directAExc a = alexDexcAem a / alexAexcAem a

-- | Correct rates for crosstalk
correctDirectAExc :: DirFactor -> Alex Double -> Alex Double
correctDirectAExc d alex =
    alex { alexDexcAem = alexDexcAem alex - dir }
  where dir = d * alexAexcAem alex

-- | Crosstalk factor from donor-only rates
crosstalkFactor :: Alex Double -> Crosstalk
crosstalkFactor alex = alexDexcAem alex / alexDexcDem alex

-- | Correct rates for crosstalk
correctCrosstalk :: Crosstalk -> Alex Double -> Alex Double
correctCrosstalk crosstalk alex =
    alex { alexDexcAem = alexDexcAem alex - lk }
  where lk = crosstalk * alexDexcAem alex

-- | @gammaFromRates crosstalk donorOnly donorAcceptor@ is an estimate
-- of gamma from count rates of donor-only and donor-acceptor populations
gammaFromRates :: Crosstalk -> Alex Double -> Alex Double -> Gamma
gammaFromRates crosstalk donorOnly donorAcceptor =
    crosstalk - alexDexcAem deltaI / alexDexcDem deltaI
  where deltaI = (-) <$> donorAcceptor <*> donorOnly
