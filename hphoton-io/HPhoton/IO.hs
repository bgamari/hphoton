module HPhoton.IO (readStamps) where

import Control.Error
import qualified HPhoton.IO.RawTimestamps as Raw

data ReadError = ReadIOException IOException
               | ParseError String
               | UnrecognizedFormat

data Metadata = Jiffy Double
              | CreationDate UTCTime
              deriving (Show, Ord)

type Channel = Int

readStamps :: FilePath -> Channel -> EitherT ReadError IO (V.Vector Time, [Metadata])
readStamps fname = tryReaders readers
    
tryReaders :: FilePath -> Channel -> [Reader]           

data Format = Format { fmtIdentify :: FilePath -> Bool
                     , fmtReader   :: FilePath -> Channel
                                   -> EitherT ReadError IO (V.Vector Time, [Metadata])
                     }

formats :: [Format]
readers = [fpgaTimetagger, picoharp, raw]

hasExtension :: [String] -> String -> Bool
hasExtension exts fname = any (`isPrefixOf` fname) exts
 
fpgaTimetagger :: Format
fpgaTimetagger = Format (hasExtension [".timetag"]) reader
  where
    reader n = undefined

rawReader :: (Identify, Reader)
rawReader = Format (hasExtension [".times", ".raw"]) reader
  where
    reader fname = do
      stamps <- Raw.readStamps fname
      return (stamps, [])
