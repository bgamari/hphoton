module HPhoton.Fret.Types ( FretChannel(..)
                          , Fret(..)
                          -- * Useful type synonyms
                          , FretEff, ProxRatio
                          , Gamma
                          ) where

type FretEff = Double
type ProxRatio = Double
type Gamma = Double

data FretChannel = Donor | Acceptor deriving (Show, Eq)

data Fret a = Fret { fretA, fretD :: a } deriving (Show, Eq)
                                                  
instance Functor Fret where
  fmap f (Fret x y) = Fret (f x) (f y)
  
