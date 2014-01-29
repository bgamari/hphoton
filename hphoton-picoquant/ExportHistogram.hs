import           HPhoton.IO.Picoquant.Interactive
      
import           Control.Applicative
import           Control.Lens hiding (argument)
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import qualified Data.Csv as Csv
import           Data.Foldable as F
import           Data.Traversable as T
import qualified Data.Vector as V
import           Options.Applicative

binStarts :: CurveHdr -> V.Vector Int
binStarts curve =
    V.generate (curve ^. curveChannels . to fromIntegral)
    $ \i->1000 * offset + i
  where offset = curve ^.curveOffset . to fromIntegral

encodeOpts :: Csv.EncodeOptions
encodeOpts =
    Csv.defaultEncodeOptions { Csv.encDelimiter = fromIntegral $ ord '\t' }

data Options = Options { fileName :: FilePath
                       , output   :: Maybe FilePath
                       }

opts :: Parser Options     
opts =
    Options <$> argument Just ( help "Name of .phd file to export"
                             <> metavar "FILE"
                              )
            <*> option ( help "Output file"
                      <> metavar "FILE"
                      <> short 'o'
                      <> long "output"
                      <> value Nothing
                      <> reader (pure . Just)
                       )
    
main = do
    o <- execParser $ info opts $ header "Export Picoharp .phd histograms"
                            
    phd <- runGet getPhdHistogram <$> LBS.readFile (fileName o)
    forM_ (phd ^. phdCurves) $ \(hdr, bins) -> do
      LBS.writeFile (maybe (fileName o++".txt") id $ output o)
        $ Csv.encodeWith encodeOpts $ V.toList $ V.zip (binStarts hdr) bins
    

