module HPhoton.IO.FpgaTimetagger.Pipes
    ( decodeRecords
    , filterDeltas
    , encodeRecords
    , module Fpga
    ) where

import           Control.Lens
import           Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import           HPhoton.IO.FpgaTimetagger as Fpga hiding (decodeRecords)

-- | Decode records
decodeRecords :: (Monad m) => Pipe BS.ByteString Record m r
decodeRecords = go BS.empty where
    go bs
        | BS.length bs < 6 = do bs' <- await
                                go (bs `BS.append` bs')
        | otherwise = do case Fpga.decodeRecord $ BU.unsafeTake 6 bs of
                             Just r  -> yield r >> go (BU.unsafeDrop 6 bs)
                             Nothing -> go (BU.unsafeDrop 6 bs)

encodeRecords :: (Monad m) => Pipe Record BS.ByteString m r
encodeRecords = PP.map Fpga.encodeRecord

-- | Drop delta records that have no strobe events after them               
filterDeltas :: (Monad m) => Pipe Record Record m r
filterDeltas = go Nothing where
    go lastDelta = do r <- await
                      if r^.recDelta
                          then go (Just r)
                          else do maybe (return ()) yield lastDelta
                                  yield r
                                  go Nothing
