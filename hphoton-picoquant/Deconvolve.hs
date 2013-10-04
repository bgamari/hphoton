{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE DataKinds, RankNTypes #-}
                
module Deconvolve where

import Data.Distributive
import Data.Traversable as T
import Data.Foldable as F
import Control.Applicative
import Linear
import Linear.V

import           Data.Vector (Vector)
import qualified Data.Vector as V

-- | @richardsonLucy psf x@ is the deconvolution of @x@
-- under point spread function @psf@.
richardsonLucy :: forall f g a.
                ( Functor f, Applicative f, Metric f
                , Functor g, Applicative g, Metric g, Distributive g
                , Fractional a, Conjugate a)
               => f (g a) -> f a -> [g a]
richardsonLucy p d = richardsonLucy' p d (pure 0.5)

-- | @richardsonLucy' psf x u0@ is the deconvolution of @x@
-- under point spread function @psf@ with initial guess @u0@.
richardsonLucy' :: forall f g a.
                 ( Functor f, Applicative f, Metric f
                 , Functor g, Applicative g, Metric g, Distributive g
                 , Fractional a, Conjugate a)
                => f (g a) -> f a -> g a -> [g a]
richardsonLucy' p d = go
  where go :: g a -> [g a]
        go u = u' : go u'
          where !c = fmap (\pi->pi `dot` u) p :: f a
                d_c :: f a
                !d_c = (/) <$> d <*> c
                f :: f a -> a -> a
                f pj uj = uj * (d_c `dot` pj)
                u' :: g a
                !u' = f <$> adjoint p <*> u
               
data V' a = V' {runV' :: forall k. Dim k => V k a}

richardsonLucyV :: (Fractional a, Conjugate a)
                => V.Vector (V.Vector a) -> V.Vector a -> [V.Vector a]
richardsonLucyV p d =
    map toVector $ richardsonLucy (vector $ fmap vector p) (vector d)
  where vector :: forall k a. Dim k => V.Vector a -> V k a
        vector v = case fromVector v of Just v' -> v'
  
