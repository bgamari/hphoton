{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TemplateHaskell #-}

module HPhoton.IO.Picoquant.PT2 where

import Data.Binary
import Data.Binary.Get
import Data.Word
import HPhoton.IO.Picoquant.Types
import Data.Traversable as T
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Typeable
import Control.Applicative
import GHC.Generics (Generic)
import Control.Lens
import qualified Data.ByteString.Lazy as LBS
  
data PT2 = PT2 { _pt2TextHdr    :: TextHdr
               , _pt2BinaryHdr  :: BinaryHdr
               , _pt2THdr       :: THdr
               , _pt2Records    :: LBS.ByteString
               }
         deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''PT2     

getPT2 :: Get PT2
getPT2 = do
    thdr <- getTextHdr
    bhdr <- getBinaryHdr
    t <- getTHdr
    records <- getLazyByteString (fromIntegral (t ^. tNumRecords) * 4)
    return $ PT2 thdr bhdr t records
