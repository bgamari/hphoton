{-# LANGUAGE ScopedTypeVariables #-}

import           Prelude hiding (mapM_)
import           Control.Applicative
import           Data.Char
import           Data.Maybe (mapMaybe)
import           Data.Foldable as F
import           Data.Traversable as T
import           System.FilePath (takeExtension)

import           Options.Applicative
import           Control.Lens hiding (argument)
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv

import           Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

import           HPhoton.IO.Picoquant.Interactive as Phd
import           HPhoton.IO.Picoquant.PT3 as PT3
import           HPhoton.IO.Picoquant.Types

type Histogram = V.Vector (Double, Int)

binStarts :: Phd.CurveHdr -> V.Vector Double
binStarts curve =
    V.generate (curve ^. curveChannels . to fromIntegral)
    $ \i->1000 * offset + 1000 * res * realToFrac i
  where
    offset = curve ^.curveOffset . to fromIntegral
    res = curve ^. curveResolution . to realToFrac

encodeOpts :: Csv.EncodeOptions
encodeOpts =
    Csv.defaultEncodeOptions { Csv.encDelimiter = fromIntegral $ ord '\t' }

data Options = Options { fileName :: FilePath
                       , output   :: Maybe FilePath
                       }

histogram :: FilePath -> Maybe FilePath -> Int -> IO ()
histogram inPath out channel = do
    let outPath = maybe (inPath++".txt") id out
    input <- LBS.readFile inPath
    let hist = case takeExtension inPath of
                   ".phd" -> readPhdHistogram input (fromIntegral channel)
                   ".pt3" -> readT3Histogram input (fromIntegral channel)
                   ext    -> fail $ "Unrecognized file format: "++ext
    LBS.writeFile outPath
        $ Csv.encodeWith encodeOpts
        $ V.toList $ either error id hist

opts :: Parser (IO ())
opts =
    subparser $
      ( command "histogram" $ info
          (histogram <$> argument str ( help "Input file"
                                     <> metavar "FILE"
                                      )
                     <*> option ( help "Output file"
                               <> metavar "FILE"
                               <> short 'o'
                               <> long "output"
                               <> value Nothing
                               <> reader (pure . Just)
                                )
                     <*> option ( help "Channel number"
                               <> metavar "N"
                               <> short 'c'
                               <> long "channel"
                               <> value 1
                                ))
          (progDesc "Export a histogram")
      )

main = do
    o <- execParser (info opts idm)
    o

readPhdHistogram :: LBS.ByteString -> Channel -> Either String Histogram
readPhdHistogram input ch = do
    let phd = runGet getPhdHistogram input
    case preview (phdCurves . ix (fromIntegral ch)) phd of
      Nothing         -> Left $ "Requested curve"
      Just (hdr, pts) -> Right $ V.zip (binStarts hdr) $ V.map fromIntegral pts

readT3Histogram :: LBS.ByteString -> Channel -> Either String Histogram
readT3Histogram input ch =
    let pt3 = runGet getPT3 input
        eventOfChannel :: T3Record -> Maybe DTime
        eventOfChannel (Event dtime _ c)
          | ch == c      = Just dtime
        eventOfChannel _ = Nothing
        h = hist $ mapMaybe eventOfChannel
                 $ VS.toList $ pt3 ^. pt3Records
        Just res = preview (pt3Boards . ix 0 . resolution) pt3
        lags = V.map (\i->realToFrac i * 1000 * realToFrac res) $ V.enumFromTo 0 (2^12)
    in Right $ V.zip lags (V.convert h)

modify :: (PrimMonad m, VU.Unbox a)
       => (a -> a) -> Int
       -> MVU.MVector (PrimState m) a -> m ()
modify f i v = MVU.read v i >>= MVU.write v i . f

hist :: forall f a. (Foldable f, Integral a, Bounded a)
     => f a -> VU.Vector Int
hist xs = VU.create $ do
    let range = (maxBound - minBound) :: a
    accum <- MVU.replicate (fromIntegral range) 0
    mapM_ (\x->modify (+1) (fromIntegral $ x - minBound) accum) xs
    return accum
