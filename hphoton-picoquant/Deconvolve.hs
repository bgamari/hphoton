{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
                
module Deconvolve where

import Data.Traversable as T
import Data.Foldable as F
import Control.Applicative
import Linear

-- | @richardsonLucy psf x@ is the deconvolution of @x@
-- under point spread function @psf@.
richardsonLucy :: forall f g a.
                ( Functor f, Applicative f, Traversable f, Metric f
                , Functor g, Applicative g, Metric g
                , RealFrac a)
               => f (g a) -> f a -> [g a]
richardsonLucy p d = richardsonLucy' p d (pure 0.5)

-- | @richardsonLucy' psf x u0@ is the deconvolution of @x@
-- under point spread function @psf@ with initial guess @u0@.
richardsonLucy' :: forall f g a.
                 ( Functor f, Applicative f, Traversable f, Metric f
                 , Functor g, Applicative g, Metric g
                 , RealFrac a)
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
                !u' = f <$> T.sequenceA p <*> u
               

