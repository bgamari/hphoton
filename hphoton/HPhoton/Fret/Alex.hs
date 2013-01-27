{-# LANGUAGE DeriveGeneric #-}

module HPhoton.Fret.Alex ( Alex(..)
                         , proxRatio
                         , fretEff
                         , stoiciometry
                         ) where

import           Control.Applicative
import qualified Data.Binary as B
import           Data.Traversable
import           Data.Foldable
import           Data.Monoid
import           GHC.Generics (Generic)

data Alex a = Alex { alexAexcAem :: a
                   , alexAexcDem :: a
                   , alexDexcAem :: a
                   , alexDexcDem :: a
                   }
            deriving (Show, Eq, Generic)
                     
instance (B.Binary a) => B.Binary (Alex a)
         
instance Functor Alex where
  fmap f a = Alex { alexAexcAem = f $ alexAexcAem a
                  , alexAexcDem = f $ alexAexcDem a
                  , alexDexcDem = f $ alexDexcDem a
                  , alexDexcAem = f $ alexDexcAem a
                  }
  
instance Applicative Alex where
  pure x = Alex x x x x
  (Alex a b c d) <*> (Alex x y z w) = Alex (a x) (b y) (c z) (d w)

instance Foldable Alex where
  foldMap f (Alex a b c d) = f a <> f b <> f c <> f d

instance Traversable Alex where
  traverse f (Alex a b c d) = Alex <$> f a <*> f b <*> f c <*> f d

fretEff :: Double -> Alex Double -> Double
fretEff gamma alex = iA / (iA + gamma*iD)
  where iA = alexDexcAem alex
        iD = alexDexcDem alex
        
proxRatio = fretEff 1

stoiciometry :: Alex Double -> Double 
stoiciometry alex = (iDexcDem + iDexcAem) / (iDexcDem + iDexcAem + iAexcDem + iAexcAem)
  where Alex { alexAexcAem = iAexcAem
             , alexAexcDem = iAexcDem
             , alexDexcDem = iDexcDem
             , alexDexcAem = iDexcAem
             } = alex
