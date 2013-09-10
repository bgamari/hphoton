{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TemplateHaskell #-}
module HPhoton.IO.Picoquant.Types where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Applicative
import GHC.Generics (Generic)
import Data.Typeable
import Control.Monad (replicateM)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Control.Lens

getEnum :: (Integral n, Enum a) => Get n -> Get a
getEnum getIntegral = fmap (toEnum . fromIntegral) getIntegral

data TextHdr = TextHdr { _ident          :: ByteString
                       , _formatVersion  :: ByteString
                       , _creatorName    :: ByteString
                       , _creatorVersion :: ByteString
                       , _fileTime       :: ByteString
                       , _comment        :: ByteString
                       }
             deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''TextHdr

getTextHdr :: Get TextHdr
getTextHdr =
    TextHdr <$> getByteString 16
            <*> getByteString 6
            <*> getByteString 18
            <*> getByteString 12
            <*> getByteString 18
            <*  getByteString 2
            <*> getByteString 256

data MeasurementMode = Interactive | T2 | T3
                     deriving (Show, Read, Ord, Eq, Typeable, Generic)

instance Enum MeasurementMode where
    fromEnum Interactive   = 0
    fromEnum T2            = 2
    fromEnum T3            = 3
    toEnum 0               = Interactive
    toEnum 2               = T2
    toEnum 3               = T3
    toEnum _               = error "Unknown measurement mode"

data SubMode = Oscilloscope | Integration | TRES
             deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)

data Range = BaseResolution | ResX2 | Resx4 | ResX8 | ResX16 | ResX32 | ResX64 | ResX128
           deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)

data LinLog = Linear | Log
            deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable, Generic)

data Parameter = Parameter { _paramStart  :: Float
                           , _paramStep   :: Float
                           , _paramEnd    :: Float
                           }
               deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''Parameter     

getParameter :: Get Parameter
getParameter =
    Parameter <$> getFloat32le
              <*> getFloat32le
              <*> getFloat32le

data DisplayCurve = DisplayCurve { _displayCurveMapTo   :: Word32
                                 , _displayCurveShow    :: Bool
                                 }
                  deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''DisplayCurve     

getDisplayCurve :: Get DisplayCurve
getDisplayCurve =
    DisplayCurve <$> getWord32le
                 <*> getEnum getWord32le

data BinaryHdr = BinaryHdr { _nCurves          :: Word32
                           , _bitsPerRecord    :: Word32
                           , _routingChannels  :: Word32
                           , _nBoards          :: Word32
                           , _activeCurve      :: Word32
                           , _measurementMode  :: MeasurementMode
                           , _subMode          :: SubMode
                           , _rangeNo          :: Range
                           , _offset           :: Word32
                           , _acqTime          :: Word32
                           , _stopAt           :: Word32
                           , _stopOnOverflow   :: Bool
                           , _restart          :: Bool
                           , _displayLinLog    :: LinLog
                           , _displayTimeAxisFrom  :: Word32
                           , _displayTimeAxisTo    :: Word32
                           , _displayCountAxisTo   :: Word32
                           , _displayCountAxisFrom :: Word32
                           , _displayCurves    :: Vector DisplayCurve
                           , _parameters       :: Vector Parameter
                           , _repeatMode       :: Word32
                           , _repeatsPerCurve  :: Word32
                           , _repeatTime       :: Word32
                           , _repeatWaitTime   :: Word32
                           , _scriptName       :: ByteString
                           }
                    deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''BinaryHdr

getBinaryHdr :: Get BinaryHdr
getBinaryHdr = do
    BinaryHdr <$> getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> getEnum getWord32le
              <*> getEnum getWord32le
              <*> getEnum getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> getEnum getWord32le
              <*> getEnum getWord32le
              <*> getEnum getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> V.replicateM 8 getDisplayCurve
              <*> V.replicateM 3 getParameter
              <*> getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> getWord32le
              <*> getByteString 20

data CfdChannel = CfdChannel { _cfdZeroCross   :: Word32
                             , _cfdLevel       :: Word32
                             }
                deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''CfdChannel

getCfdChannel :: Get CfdChannel
getCfdChannel =
    CfdChannel <$> getWord32le <*> getWord32le

data RouterInputType = RouterInputCustom | RouterInputNIM | RouterInputTTL
                     deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable, Generic)

data RisingFalling = Rising | Falling
                     deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable, Generic)

data RouterChannel = RouterChannel { _routerInputType     :: RouterInputType
                                   , _routerInputLevel    :: Word32
                                   , _routerInputEdge     :: RisingFalling
                                   , _routerCfdPresent    :: Bool
                                   , _routerCfdLevel      :: Word32
                                   , _routerCfdZeroCross  :: Word32
                                   }
                   deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''RouterChannel     

getRouterChannel :: Get RouterChannel
getRouterChannel =
    RouterChannel <$> getEnum getWord32le
                  <*> getWord32le
                  <*> getEnum getWord32le
                  <*> getEnum getWord32le
                  <*> getWord32le
                  <*> getWord32le

data BoardHdr = BoardHdr { _hardwareIdent      :: ByteString
                         , _hardwareVersion    :: ByteString
                         , _hardwareSerial     :: Word32
                         , _syncDivider        :: Word32
                         , _cfdChannels        :: Vector CfdChannel
                         , _resolution         :: Float
                         , _routerModelCode    :: Word32
                         , _routerEnabled      :: Bool
                         , _routerChannels     :: Vector RouterChannel
                         }
              deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''BoardHdr

getBoardHdr :: Get BoardHdr
getBoardHdr =
    BoardHdr <$> getByteString 16
             <*> getByteString 8
             <*> getWord32le
             <*> getWord32le
             <*> V.replicateM 2 getCfdChannel
             <*> getFloat32le
             <*> getWord32le
             <*> getEnum getWord32le
             <*> V.replicateM 4 getRouterChannel

data THdr = THdr { _tExtDevices    :: Word32
                 , _tInpRate0      :: Word32
                 , _tInpRate1      :: Word32
                 , _tStopAfter     :: Word32
                 , _tStopReason    :: Word32
                 , _tNumRecords    :: Word32
                 , _tImgHdrSize    :: Word32
                 }
          deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''THdr

getTHdr :: Get THdr
getTHdr = 
    THdr <$> getWord32le           
         <*> getWord32le
         <*> getWord32le
         <*> getWord32le
         <*> getWord32le
         <*> getWord32le
         <*> getWord32le
