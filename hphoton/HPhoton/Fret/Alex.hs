{-# LANGUAGE DeriveGeneric #-}

module HPhoton.Fret.Alex ( Alex(..)
                         , proxRatio
                         , fretEff
                         , stoiciometry
                         ) where

import GHC.Generics (Generic)
import qualified Data.Binary as B
import Control.Applicative       

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
