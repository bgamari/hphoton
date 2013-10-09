module HPhoton.IO ( ReadError(..)
                  , Metadata(..)
                  , Channel
                  , readStamps
                  ) where

import Data.List
import Data.Time.Clock
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Control.Exception.Base (IOException)
import Control.Monad.Trans.Class
import Control.Error
import Control.Applicative
import HPhoton.Types
import qualified HPhoton.IO.RawTimestamps as Raw

data ReadError = ReadIOException IOException
               | ParseError String
               | InvalidChannel
               | UnrecognizedFormat
               deriving (Show)

data Metadata = Jiffy Double
              | CreationTime UTCTime
              deriving (Show)

type Channel = Int

readStamps :: FilePath -> Channel
           -> EitherT ReadError IO (VU.Vector Time, [Metadata])
readStamps = readStamps' formats
    
readStamps' :: [Format] -> FilePath -> Channel
            -> EitherT ReadError IO (VU.Vector Time, [Metadata])
readStamps' [] _ _ = left UnrecognizedFormat
readStamps' (reader:rest) fname channel
  | (fmtIdentify reader) fname = do
      res <- lift $ runEitherT $ (fmtReader reader) fname channel
      case res of
        Left UnrecognizedFormat   -> readStamps' rest fname channel
        Left err                  -> left err
        Right ret                 -> return ret
  | otherwise = readStamps' rest fname channel

data Format = Format { fmtIdentify :: FilePath -> Bool
                     , fmtReader   :: FilePath -> Channel
                                   -> EitherT ReadError IO (VU.Vector Time, [Metadata])
                     }

formats :: [Format]
formats = [fpgaTimetagger, picoharp, raw]

hasExtension :: [String] -> String -> Bool
hasExtension exts fname = any (`isPrefixOf` fname) exts
 
fpgaTimetagger :: Format
fpgaTimetagger = Format (hasExtension [".timetag"]) reader
  where
    reader fname channel = undefined

raw :: Format
raw = Format (hasExtension [".times", ".raw"]) reader
  where
    reader fname channel = do
      stamps <- VU.convert <$> lift (Raw.readStamps fname)
      return (stamps, [])

picoharp :: Format
picoharp = Format (hasExtension [".pt2", ".pt3"]) reader
  where
    reader fname channel = undefined
