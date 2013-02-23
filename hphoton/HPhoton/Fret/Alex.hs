{-# LANGUAGE DeriveGeneric #-}

module HPhoton.Fret.Alex ( Alex(..)
                         , proxRatio
                         , fretEff
                         , stoiciometry
                         , stoiciometry'
                         , directAExc
                         ) where

import           Control.Applicative
import qualified Data.Binary as B
import           Data.Traversable
import           Data.Foldable
import           Data.Monoid
import           GHC.Generics (Generic)

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

-- | Lee's 'Dir' acceptor direct excitation factor
directAExc :: Alex Double -> Double
directAExc a = alexDexcAem a / alexAexcAem a
