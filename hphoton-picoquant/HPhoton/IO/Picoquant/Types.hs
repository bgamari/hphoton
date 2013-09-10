{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module HPhoton.IO.Picoquant.Types where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Applicative
import GHC.Generics
import Data.Typeable
import Control.Monad (replicateM)
import qualified Data.Vector as V
import Data.Vector (Vector)

getEnum :: (Integral n, Enum a) => Get n -> Get a
getEnum getIntegral = fmap (toEnum . fromIntegral) getIntegral

data TextHdr = TextHdr { ident          :: ByteString
                       , formatVersion  :: ByteString
                       , creatorName    :: ByteString
                       , creatorVersion :: ByteString
                       , fileTime       :: ByteString
                       , comment        :: ByteString
                       }
             deriving (Show, Read, Eq, Ord, Typeable, Generic)

getTextHdr :: Get TextHdr
getTextHdr =
    TextHdr <$> getByteString 16
            <*> getByteString 16
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

data Parameter = Parameter { paramStart  :: Float
                           , paramStep   :: Float
                           , paramEnd    :: Float
                           }
               deriving (Show, Read, Eq, Ord, Typeable, Generic)

getParameter :: Get Parameter
getParameter =
    Parameter <$> getFloat32le
              <*> getFloat32le
              <*> getFloat32le

data DisplayCurve = DisplayCurve { mapTo            :: Word32
                                 , showCurve        :: Bool
                                 }
                  deriving (Show, Read, Eq, Ord, Typeable, Generic)

getDisplayCurve :: Get DisplayCurve
getDisplayCurve =
    DisplayCurve <$> getWord32le
                 <*> getEnum getWord32le

data BinaryHdr = BinaryHdr { nCurves          :: Word32
                           , bitsPerRecord    :: Word32
                           , routingChannels  :: Word32
                           , nBoards          :: Word32
                           , activeCurve      :: Word32
                           , measurementMode  :: MeasurementMode
                           , subMode          :: SubMode
                           , rangeNo          :: Range
                           , offset           :: Word32
                           , acqTime          :: Word32
                           , stopAt           :: Word32
                           , stopOnOverflow   :: Bool
                           , restart          :: Bool
                           , displayLinLog    :: LinLog
                           , displayTimeAxisFrom  :: Word32
                           , displayTimeAxisTo    :: Word32
                           , displayCountAxisTo   :: Word32
                           , displayCountAxisFrom :: Word32
                           , displayCurves    :: Vector DisplayCurve
                           , parameters       :: Vector Parameter
                           , repeatMode       :: Word32
                           , repeatsPerCurve  :: Word32
                           , repeatTime       :: Word32
                           , repeatWaitTime   :: Word32
                           , scriptName       :: ByteString
                           }
                    deriving (Show, Read, Eq, Ord, Typeable, Generic)

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

data CfdChannel = CfdChannel { cfdZeroCross   :: Word32
                             , cfdLevel       :: Word32
                             }
                deriving (Show, Read, Eq, Ord, Typeable, Generic)

getCfdChannel :: Get CfdChannel
getCfdChannel =
    CfdChannel <$> getWord32le <*> getWord32le

data RouterInputType = RouterInputCustom | RouterInputNIM | RouterInputTTL
                     deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable, Generic)

data RisingFalling = Rising | Falling
                     deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable, Generic)

data RouterChannel = RouterChannel { routerInputType     :: RouterInputType
                                   , routerInputLevel    :: Word32
                                   , routerInputEdge     :: RisingFalling
                                   , routerCfdPresent    :: Bool
                                   , routerCfdLevel      :: Word32
                                   , routerCfdZeroCross  :: Word32
                                   }
                   deriving (Show, Read, Eq, Ord, Typeable, Generic)

getRouterChannel :: Get RouterChannel
getRouterChannel =
    RouterChannel <$> getEnum getWord32le
                  <*> getWord32le
                  <*> getEnum getWord32le
                  <*> getEnum getWord32le
                  <*> getWord32le
                  <*> getWord32le

data BoardHdr = BoardHdr { hardwareIdent      :: ByteString
                         , hardwareVersion    :: ByteString
                         , hardwareSerial     :: Word32
                         , syncDivider        :: Word32
                         , cfdChannels        :: Vector CfdChannel
                         , resolution         :: Float
                         , routerModelCode    :: Word32
                         , routerEnabled      :: Bool
                         , routerChannels     :: Vector RouterChannel
                         }
              deriving (Show, Read, Eq, Ord, Typeable, Generic)

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
             <*> V.replicateM 8 getRouterChannel

data CurveHdr = CurveHdr { curveIndex           :: Word32
                         , curveTimeOfRecording :: Word32
                         , curveHardwareIdent   :: ByteString
                         , curveHardwareVersion :: ByteString
                         , curveHardwareSerial  :: Word32
                         , curveSyncDivider     :: Word32
                         , curveCfdChannels     :: Vector CfdChannel
                         , curveOffset          :: Word32
                         , curveRoutingChannel  :: Word32
                         , curveExtDevices      :: Word32
                         , curveMeasurementMode :: MeasurementMode
                         , curveSubMode         :: SubMode
                         , curveP1              :: Float
                         , curveP2              :: Float
                         , curveP3              :: Float
                         , curveRange           :: Range
                         , curveResolution      :: Float
                         , curveChannels        :: Word32
                         , curveAcqTime         :: Word32
                         , curveStopAfter       :: Word32
                         , curveStopReason      :: Word32
                         , curveInpRate0        :: Word32
                         , curveInpRate1        :: Word32
                         , curveHistCountRate   :: Word32
                         , curveIntegralCount   :: Word64
                         , curveDataOffset      :: Word32
                         , curveRouterModelCode :: Word32
                         , curveRouterEnabled   :: Bool
                         , curveRouterChannel   :: RouterChannel
                         }
              deriving (Show, Read, Eq, Ord, Typeable, Generic)

getCurveHdr :: Get CurveHdr
getCurveHdr =
    CurveHdr <$> getWord32le
             <*> getWord32le
             <*> getByteString 16
             <*> getByteString 8
             <*> getWord32le
             <*> getWord32le
             <*> V.replicateM 2 getCfdChannel
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getEnum getWord32le
             <*> getEnum getWord32le
             <*> getFloat32le
             <*> getFloat32le
             <*> getFloat32le
             <*> getEnum getWord32le
             <*> getFloat32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord64le
             <*> getWord32le
             <*> getWord32le
             <*> getEnum getWord32le
             <*> getRouterChannel
