{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HPhoton.IO.Picoquant.PT3 where

import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Word
import HPhoton.IO.Picoquant.Types
import Data.Traversable as T
import qualified Data.Vector as V
import Data.Typeable
import Control.Applicative
import GHC.Generics (Generic)
import Control.Lens

import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr)
import qualified Data.ByteString.Unsafe as BS
import Data.Vector (Vector)
import qualified Data.Vector.Storable as VS
import Foreign.Storable
import Foreign.ForeignPtr (castForeignPtr)
import Foreign.Ptr (castPtr, Ptr)

type Channel = Word8
type DTime = Word16

data T3Record = Event { _dTime :: DTime
                      , _nSync :: Word16
                      , _channel :: Channel
                      }
              | Overflow { _nSync :: Word16 }
              | Marker { _markers :: Word16
                       , _nSync :: Word16
                       }
              deriving (Show, Read, Eq, Ord)
makeLenses ''T3Record

instance Storable T3Record where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        n <- peek $ castPtr p :: IO Word32
        let dtime = fromIntegral $ (n `shiftR` 16) .&. 0x0fff
            nsync = fromIntegral $ n .&. 0xffff
        return $ case (n `shiftR` 28) .&. 0xf of
                     0xf | dtime == 0 -> Overflow nsync
                         | otherwise  -> Marker dtime nsync
                     ch               -> Event dtime nsync (fromIntegral ch)

    poke _ (Event dtime _ _) | dtime >= 2^12 = error "Invalid dtime"
    poke _ (Event _ nsync _) | nsync >= 2^16 = error "Invalid nsync"
    poke _ (Event _ _ ch)    | ch    >= 2^4  = error "Invalid channel"
    poke _ (Overflow nsync)  | nsync >= 2^16 = error "Invalid nsync"
    poke _ (Overflow m)      | m     >= 2^12 = error "Invalid markers"
    poke p r = do
        let chan = case r of
                       Event _ _ n -> fromIntegral n
                       Overflow _  -> 0xf
                       Marker _ _  -> 0xf
        let dtime = case r of
                        Event n _ _ -> fromIntegral n
                        Overflow _  -> 0
                        Marker n _  -> fromIntegral n
        let nsync = case r of
                        Event _ n _ -> fromIntegral n
                        Overflow n  -> fromIntegral n
                        Marker _ n  -> fromIntegral n
        poke (castPtr p :: Ptr Word32)
          $ chan `shiftL` 28 .|. dtime `shiftL` 16 .|. nsync

data PT3 = PT3 { _pt3TextHdr    :: TextHdr
               , _pt3BinaryHdr  :: BinaryHdr
               , _pt3Boards     :: V.Vector BoardHdr
               , _pt3THdr       :: THdr
               , _pt3Records    :: VS.Vector T3Record
               }
         deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''PT3

getPT3 :: Get PT3
getPT3 = do
    thdr <- getTextHdr
    bhdr <- getBinaryHdr
    boards <- V.replicateM (bhdr ^. nBoards . to fromIntegral)
                           getBoardHdr
    t <- getTHdr
    let nRecs = t ^. tNumRecords
        recSize = fromIntegral $ sizeOf (undefined :: T3Record)
    recBS <- getByteString (fromIntegral nRecs * recSize)
    let (fptr, offset, _) = toForeignPtr recBS
    let records = VS.unsafeFromForeignPtr (castForeignPtr fptr)
                                          (fromIntegral offset)
                                          (fromIntegral nRecs)
    return $ PT3 thdr bhdr boards t records
