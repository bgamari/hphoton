module HPhoton.Fret.Types ( FretChannel(..)
                          , Fret(..)
                          -- * Useful type synonyms
                          , FretEff, ProxRatio
                          , Gamma
                          ) where

import Data.Foldable
import Data.Monoid

type FretEff = Double
type ProxRatio = Double
type Gamma = Double

data FretChannel = Donor | Acceptor deriving (Show, Eq)

data Fret a = Fret { fretA, fretD :: a } deriving (Show, Eq)
                                                  
instance Functor Fret where
  fmap f (Fret x y) = Fret (f x) (f y)
  
instance Foldable Fret where
  foldMap f (Fret x y) = f x <> f y
