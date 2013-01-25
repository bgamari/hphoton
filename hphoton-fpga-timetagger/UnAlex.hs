import           System.IO
import           HPhoton.FpgaTimetagger.Alex
import           HPhoton.FpgaTimetagger.Pipe
import           HPhoton.Fret.Alex
import           HPhoton.Types

import           Control.Proxy as P
import qualified Control.Proxy.ByteString as PBS
import Control.Proxy.Trans.Writer
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Lens

filterDeltas :: (Monad m, Proxy p) => () -> Pipe p Record Record m ()
filterDeltas () = runIdentityP $ go Nothing where
    go lastDelta = do r <- P.request ()
                      if r^.recDelta
                          then go (Just r)
                          else do maybe (return ()) P.respond lastDelta
                                  P.respond r
                                  go Nothing

main = do
    withFile "/home/ben/lori/data/rna/solution-fret/2013-01-21/2013-01-21-run_004.timetag" ReadMode $ \fIn->do
    withFile "/home/ben/lori/data/rna/solution-fret/2013-01-21/2013-01-21-run_004-trimmed.timetag" WriteMode $ \fOut->do
    a <- runProxy $ PBS.fromHandleS fIn >-> decodeRecordsP >-> filterDeltas >-> encodeRecordsP >-> PBS.toHandleD fOut
    print a
