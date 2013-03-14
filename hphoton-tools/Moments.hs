{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Moments ( Moments
               , mean
               , variance
               , sample
               ) where

-- | Online calculation of moments. Concept shamelessly borrowed from
-- Edward Kmett's 'analytics'.
-- .
-- Online moment calculation due to Chan 1979

import Data.Monoid
import Data.Foldable

-- | Monoid computation of mean and variance of univariate samples
data Moments a = Moments { count        :: !Int
                         , mean         :: !a
                         , rawVariance  :: !a
                         } deriving (Read, Show)

instance (Num a, Fractional a) => Monoid (Moments a) where
    mempty = Moments 0 0 0
    Moments na ma va `mappend` Moments nb mb vb = Moments
        (na + nb)                           -- count
        ((na'*ma + nb'*mb) / (na' + nb'))   -- mean
        (va + vb + a * d^2)                 -- variance
      where d = mb - ma
            na' = fromIntegral na
            nb' = fromIntegral nb
            a = (na' * nb') / (na' + nb')

sample :: Num a => a -> Moments a
sample x = Moments 1 x 0

-- | The moments of a structure of samples
momentsOf :: (Foldable f, Fractional a) => f a -> Moments a
momentsOf = foldMap' sample

-- | Strict foldMap
foldMap' :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap' f = foldl' (\m a->mappend m $! f a) mempty

variance :: (Fractional a) => Moments a -> a
variance (Moments n _ v) = v / fromIntegral n
