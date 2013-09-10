{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module HPhoton.IO.Picoquant where

import Data.Binary
import Data.Binary.Get
import Data.Word
import HPhoton.IO.Picoquant.Types
import Data.Traversable as T
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Typeable
import GHC.Generics

data PhdHistogram = PhdHistogram { phdTextHdr   :: TextHdr
                                 , phdBinaryHdr :: BinaryHdr
                                 , phdBoards    :: Vector BoardHdr
                                 , phdCurves    :: Vector (CurveHdr, Vector Word32)
                                 }
                  deriving (Show, Read, Eq, Ord, Typeable, Generic)


getPhdHistogram :: Get PhdHistogram
getPhdHistogram = do
    thdr <- getTextHdr
    bhdr <- getBinaryHdr
    boards <- V.replicateM (fromIntegral $ nBoards bhdr) getBoardHdr
    curves <- V.replicateM (fromIntegral $ nCurves bhdr) getCurveHdr
    histograms <- T.forM curves $ \curve->do
        V.replicateM (fromIntegral $ curveChannels curve) getWord32le
    return $ PhdHistogram thdr bhdr boards (V.zip curves histograms)
