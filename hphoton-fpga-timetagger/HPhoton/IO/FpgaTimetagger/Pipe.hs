module HPhoton.IO.FpgaTimetagger.Pipe ( decodeRecordsP
                                      , filterDeltasP
                                      , encodeRecordsP
                                      , module HPhoton.IO.FpgaTimetagger
                                      ) where

import           Control.Lens
import           Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import           HPhoton.IO.FpgaTimetagger

-- | Decode records
decodeRecordsP :: (Monad m) => Pipe BS.ByteString Record m r
decodeRecordsP = go BS.empty where
    go bs
        | BS.length bs < 6 = do bs' <- await
                                go (bs `BS.append` bs')
        | otherwise = do case decodeRecord $ BU.unsafeTake 6 bs of
                             Just r  -> yield r >> go (BU.unsafeDrop 6 bs)
                             Nothing -> go (BU.unsafeDrop 6 bs)

encodeRecordsP :: (Monad m) => Pipe Record BS.ByteString m r
encodeRecordsP = P.map encodeRecord

-- | Drop delta records that have no strobe events after them               
filterDeltasP :: (Monad m) => Pipe Record Record m r
filterDeltasP = go Nothing where
    go lastDelta = do r <- await
                      if r^.recDelta
                          then go (Just r)
                          else do maybe (return ()) yield lastDelta
                                  yield r
                                  go Nothing
