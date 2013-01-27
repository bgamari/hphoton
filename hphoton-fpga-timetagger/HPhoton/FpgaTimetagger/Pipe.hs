module HPhoton.FpgaTimetagger.Pipe ( decodeRecordsP
                                   --, readRecordsP
                                   , filterDeltasP
                                   , encodeRecordsP
                                   , module HPhoton.FpgaTimetagger
                                   ) where

import           Control.Lens
import           Control.Proxy as P
import qualified Control.Proxy.ByteString as PBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import           HPhoton.FpgaTimetagger

-- | Decode records
decodeRecordsP :: (Monad m, Proxy p) => () -> Pipe p BS.ByteString Record m ()
decodeRecordsP = P.runIdentityK (go BS.empty) where
    go bs
        | BS.length bs < 6 = \x -> do bs' <- P.request x
                                      go (bs `BS.append` bs') x
        | otherwise = \x -> do case decodeRecord $ BU.unsafeTake 6 bs of
                                   Just r  -> P.respond r >>= go (BU.unsafeDrop 6 bs)
                                   Nothing -> go (BU.unsafeDrop 6 bs) x
    
-- | Read records from a file
--readRecordsP :: (Monad m, Proxy p) => FilePath -> () -> Producer p Record m ()
--readRecordsP fname = PBS.fromHandleS fname >-> decodeRecordsP

encodeRecordsP :: (Monad m, Proxy p) => () -> Pipe p Record BS.ByteString m ()
encodeRecordsP = mapD encodeRecord

-- | Drop delta records that have no strobe events after them               
filterDeltasP :: (Monad m, Proxy p) => () -> Pipe p Record Record m ()
filterDeltasP () = runIdentityP $ go Nothing where
    go lastDelta = do r <- P.request ()
                      if r^.recDelta
                          then go (Just r)
                          else do maybe (return ()) P.respond lastDelta
                                  P.respond r
                                  go Nothing