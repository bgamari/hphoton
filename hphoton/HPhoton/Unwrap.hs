module HPhoton.Unwrap (unwrapTimes) where

import           Control.Monad.Trans.State
import qualified Data.Vector.Generic         as G
import           HPhoton.Types

data UnwrapState a = US { offset :: !a
                        , lastT  :: !a
                        }
                      
-- | Fix timing wraparounds
unwrapTimes :: (Ord a, Num a, G.Vector v a) => a -> v a -> v a
unwrapTimes maxT ts = evalState (G.mapM f ts) (US 0 0)
  where f t = do US offset lastT <- get
                 let offset' = if t < lastT
                                   then offset+maxT
                                   else offset
                 put $ US offset' t
                 return $! t + offset'
{-# INLINABLE unwrapTimes #-}
