{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TemplateHaskell #-}
module HPhoton.IO.Picoquant where

import Data.Binary
import Data.Binary.Get
import Data.Word
import HPhoton.IO.Picoquant.Types
import Data.Traversable as T
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Typeable
import GHC.Generics (Generic)
import Control.Lens

data PhdHistogram = PhdHistogram { _phdTextHdr   :: TextHdr
                                 , _phdBinaryHdr :: BinaryHdr
                                 , _phdBoards    :: Vector BoardHdr
                                 , _phdCurves    :: Vector (CurveHdr, Vector Word32)
                                 }
                  deriving (Show, Read, Eq, Ord, Typeable, Generic)


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
