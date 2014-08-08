{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TemplateHaskell #-}

-- | Read interactive (.phd) histogram
module HPhoton.IO.Picoquant.Interactive where

import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.ByteString (ByteString)
import Data.Word
import HPhoton.IO.Picoquant.Types
import Data.Traversable as T
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Typeable
import Control.Applicative
import GHC.Generics (Generic)
import Control.Lens

data CurveHdr = CurveHdr { _curveIndex           :: Word32
                         , _curveTimeOfRecording :: Word32
                         , _curveHardwareIdent   :: ByteString
                         , _curveHardwareVersion :: ByteString
                         , _curveHardwareSerial  :: Word32
                         , _curveSyncDivider     :: Word32
                         , _curveCfdChannels     :: Vector CfdChannel
                         , _curveOffset          :: Word32
                         , _curveRoutingChannel  :: Word32
                         , _curveExtDevices      :: Word32
                         , _curveMeasurementMode :: MeasurementMode
                         , _curveSubMode         :: SubMode
                         , _curveP1              :: Float
                         , _curveP2              :: Float
                         , _curveP3              :: Float
                         , _curveRange           :: Range
                         , _curveResolution      :: Float
                         , _curveChannels        :: Word32
                         , _curveAcqTime         :: Word32
                         , _curveStopAfter       :: Word32
                         , _curveStopReason      :: Word32
                         , _curveInpRate0        :: Word32
                         , _curveInpRate1        :: Word32
                         , _curveHistCountRate   :: Word32
                         , _curveIntegralCount   :: Word64
                         ,  curveReserved        :: Word32
                         , _curveDataOffset      :: Word32
                         , _curveRouterModelCode :: Word32
                         , _curveRouterEnabled   :: Bool
                         , _curveRouterChannel   :: RouterChannel
                         }
              deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''CurveHdr

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
             <*> getWord32le
             <*> getEnum getWord32le
             <*> getRouterChannel

data PhdHistogram = PhdHistogram { _phdTextHdr   :: TextHdr
                                 , _phdBinaryHdr :: BinaryHdr
                                 , _phdBoards    :: Vector BoardHdr
                                 , _phdCurves    :: Vector (CurveHdr, Vector Word32)
                                 }
                  deriving (Show, Read, Eq, Ord, Typeable, Generic)
makeLenses ''PhdHistogram

getPhdHistogram :: Get PhdHistogram
getPhdHistogram = do
    thdr <- getTextHdr
    bhdr <- getBinaryHdr
    boards <- V.replicateM (bhdr ^. nBoards . to fromIntegral) getBoardHdr
    curves <- V.replicateM (bhdr ^. nCurves . to fromIntegral) getCurveHdr
    histograms <- T.forM curves $ \curve->do
        V.replicateM (curve ^. curveChannels . to fromIntegral)
            $ getWord32le
    return $ PhdHistogram thdr bhdr boards (V.zip curves histograms)