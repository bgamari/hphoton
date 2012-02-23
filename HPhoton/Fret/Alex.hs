{-# LANGUAGE DeriveGeneric #-}

module HPhoton.Fret.Alex ( Alex(..)
                         , proximityRatio
                         , fretEff
                         , stoiciometry
                         ) where

import GHC.Generics
import qualified Data.Serialize as S

data Alex a = Alex { alexAexcAem :: a
                   , alexAexcDem :: a
                   , alexDexcAem :: a
                   , alexDexcDem :: a
                   }
            deriving (Show, Eq, Generic)
                     
instance (S.Serialize a) => S.Serialize (Alex a)
         
instance Functor Alex where
  fmap f a = Alex { alexAexcAem = f $ alexAexcAem a
                  , alexAexcDem = f $ alexAexcDem a
                  , alexDexcDem = f $ alexDexcDem a
                  , alexDexcAem = f $ alexDexcAem a
                  }
  
fretEff :: Double -> Alex Double -> Double
fretEff gamma alex = iA / (iA + gamma*iD)
  where iA = alexDexcAem alex
        iD = alexDexcDem alex
        
proximityRatio = fretEff 1

stoiciometry :: Alex Double -> Double 
stoiciometry alex = (iDexcDem + iDexcAem) / (iDexcDem + iDexcAem + iAexcDem + iAexcAem)
  where Alex { alexAexcAem = iAexcAem
             , alexAexcDem = iAexcDem
             , alexDexcDem = iDexcDem
             , alexDexcAem = iDexcAem
             } = alex
