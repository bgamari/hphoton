module HPhoton.Unwrap ( unwrapTimes
                        -- * Monadic building blocks
                      , UnwrapState
                      , initialUnwrapState
                      , unwrapTime
                      ) where

import           Control.Monad.Trans.State
import qualified Data.Vector.Generic         as G
import           HPhoton.Types

data UnwrapState a = US { offset :: !a
                        , lastT  :: !a
                        }

unwrapTime :: (Ord a, Num a, Monad m)
           => a -> a -> StateT (UnwrapState a) m a
unwrapTime maxT t = do
    US offset lastT <- get
    let offset' = if t < lastT
                     then offset+maxT
                     else offset
    put $! US offset' t
    return $! t + offset'
{-# INLINABLE unwrapTime #-}

initialUnwrapState :: Num a => UnwrapState a
initialUnwrapState = US 0 0
{-# INLINABLE initialUnwrapState #-}

-- | Fix timing wraparounds
unwrapTimes :: (Ord a, Num a, G.Vector v a) => a -> v a -> v a
unwrapTimes maxT ts =
    evalState (G.mapM (unwrapTime maxT) ts) initialUnwrapState
{-# INLINABLE unwrapTimes #-}
