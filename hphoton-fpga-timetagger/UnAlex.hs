import           System.IO
import           HPhoton.IO.FpgaTimetagger.Alex
import           HPhoton.IO.FpgaTimetagger.Pipe
import           HPhoton.Fret.Alex
import           HPhoton.Types

import           Control.Proxy as P
import qualified Control.Proxy.ByteString as PBS
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Control.Lens

main = do
    withFile "/home/ben/lori/data/rna/solution-fret/2013-01-21/2013-01-21-run_004.timetag" ReadMode $ \fIn->do
    withFile "/home/ben/lori/data/rna/solution-fret/2013-01-21/2013-01-21-run_004-trimmed.timetag" WriteMode $ \fOut->do
    a <- runProxy $ PBS.fromHandleS fIn >-> decodeRecordsP >-> filterDeltasP >-> encodeRecordsP >-> PBS.toHandleD fOut
    print a
