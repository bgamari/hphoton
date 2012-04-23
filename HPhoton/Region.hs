{-# LANGUAGE FlexibleInstances #-}

module HPhoton.Region ( Region(..)
                      , elem
                      , toUnions
                      ) where

import Prelude hiding (elem)       
import Data.Monoid       
import HPhoton.Types

data Region a = Interval (a,a)
              | Union (Region a) (Region a)
              | Diff (Region a) (Region a)
              | Intersection (Region a) (Region a)
              | Null
              deriving (Show, Eq)

elem :: Ord a => a -> Region a -> Bool
a `elem` (Span (x,y))        = a >= x && a <= y
a `elem` (Union x y)         = a `elem` x || a `elem` y
a `elem` (Diff x y)          = a `elem` x || not (a `elem` y)
a `elem` (Intersection x y)  = a `elem` x && a `elem` y
_ `elem` _                    = False

toUnions :: Ord a => Region a -> Region a
toUnions (Diff (Span (a,b)) s@(Span (c,d)))
    | a<c && b>d              = Union (Span (a,c)) (Span (d,b))
    | a<c && b `elem` s       = Span (a,c)
    | a `elem` s && b<d       = Span (d,b)
    | otherwise               = Span (a,b)
toUnions (Diff x y)           = let Union y1 y2 = toUnions y
                                in toUnions $ Union (Diff x y1) (Diff x y2)
toUnions (Intersection (Span (a,b)) (Span (c,d)))
                              = Span (max a c, min b d)
toUnions (Intersection x y)   = let Union y1 y2 = toUnions y
                                in toUnions $ Union (Intersection x y1) (Intersection x y2)
toUnions a                    = a
         
instance (Ord a) => Monoid (Sum (Region a)) where
    mempty = Sum Null
    (Sum x) `mappend` (Sum y) = Sum $ Union x y
