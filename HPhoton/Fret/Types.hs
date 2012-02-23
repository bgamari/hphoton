module HPhoton.Fret.Types ( FretChannel(..)
                          , Fret(..)
                          ) where

data FretChannel = Donor | Acceptor deriving (Show, Eq)

data Fret a = Fret { fretA, fretD :: a } deriving (Show, Eq)
                                                  
instance Functor Fret where
  fmap f (Fret x y) = Fret (f x) (f y)
  
