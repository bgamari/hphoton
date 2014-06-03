{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module HPhoton.IO.Metadata where

import Control.Lens
import Data.Time.Clock

data Metadata = Jiffy Double
              | CreationTime UTCTime
              deriving (Show)
makePrisms ''Metadata

lookupMetadata :: (Prism' Metadata a) -> [Metadata] -> Maybe a
lookupMetadata p = firstOf (traverse . p)
