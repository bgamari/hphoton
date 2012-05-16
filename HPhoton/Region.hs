{-# LANGUAGE FlexibleInstances #-}

module HPhoton.Region ( Region(..)
                      , elem
                      , toIntervals
                      , fromIntervals
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
a `elem` (Interval (x,y))    = a `elemI` (x,y)
a `elem` (Union x y)         = a `elem` x || a `elem` y
a `elem` (Diff x y)          = a `elem` x || not (a `elem` y)
a `elem` (Intersection x y)  = a `elem` x && a `elem` y
_ `elem` _                    = False

elemI :: Ord a => a -> (a,a) -> Bool
elemI x (a,b) = x >= a && x <= b

subtractI :: Ord a => (a,a) -> (a,a) -> [(a,a)]
subtractI s@(a,b) (c,d)
    | (a,b) == (c,d)             = []
    | c `elemI` s && d>b         = [(a,c)]
    | c<a && d `elemI` s         = [(d,b)]
    | c `elemI` s && d `elemI` s = [(a,c), (d,b)]
    | otherwise                  = [(a,b)]

intersectI :: Ord a => (a,a) -> (a,a) -> [(a,a)]
intersectI s@(a,b) t@(c,d)
    | (a,b) == (c,d)             = [(a,b)]                   
    | c `elemI` s && d>b         = [(c,b)]
    | c<a && d `elemI` s         = [(a,d)]
    | c `elemI` s && d `elemI` s = [(c,d)]
    | a `elemI` t && b `elemI` t = [(a,b)]
    | otherwise                  = []

fromIntervals :: Ord a => [(a,a)] -> Region a
fromIntervals [x]      = Interval x              
fromIntervals (x:rest) = Union (Interval x) (fromIntervals rest)

toIntervals :: Ord a => Region a -> [(a,a)]
toIntervals (Interval (a,b))   = [(a,b)]
toIntervals (Union x y)        = toIntervals x ++ toIntervals y
toIntervals (Diff x y)         = concat $ do (a,b) <- toIntervals x
                                             (c,d) <- toIntervals y
                                             return $ (a,b) `subtractI` (c,d)
toIntervals (Intersection x y) = concat $ do (a,b) <- toIntervals x
                                             (c,d) <- toIntervals y
                                             return $ (a,b) `intersectI` (c,d)
toIntervals Null               = []

toUnions :: Ord a => Region a -> Region a
toUnions (Diff (Interval (a,b)) s@(Interval (c,d)))
    | a<c && b>d              = Union (Interval (a,c)) (Interval (d,b))
    | a<c && b `elem` s       = Interval (a,c)
    | a `elem` s && b<d       = Interval (d,b)
    | otherwise               = Interval (a,b)
toUnions (Diff x y)           = let y' = toUnions y
                                in toUnions $ Union (Diff x y') (Diff x y')
toUnions (Intersection (Interval (a,b)) (Interval (c,d)))
                              = Interval (max a c, min b d)
toUnions (Intersection x y)   = let Union y1 y2 = toUnions y
                                in toUnions $ Union (Intersection x y1) (Intersection x y2)
toUnions (Union (Interval (x,y)) (Interval (x',y')))
    | y == x'                 = Interval (x, y')
toUnions (Union x y)          = Union (toUnions x) (toUnions y)
toUnions a                    = a
         
instance (Ord a) => Monoid (Sum (Region a)) where
    mempty = Sum Null
    (Sum x) `mappend` (Sum y) = Sum $ Union x y
