{-# LANGUAGE FlexibleContexts #-}

module HPhoton.IO ( -- * Reading timestamps
                    Channel
                  , readStamps
                  , ReadError(..)
                    -- * Metadata
                  , module HPhoton.IO.Metadata
                  ) where

import Data.List
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Control.Exception.Base (IOException)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Error
import Control.Applicative
import Control.Lens
import HPhoton.Types

import System.IO (withFile, IOMode(ReadMode))
import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS
import qualified Pipes.Vector as PV

import qualified HPhoton.IO.RawTimestamps as Raw
import qualified HPhoton.IO.FpgaTimetagger.Pipes as Fpga
import HPhoton.IO.Metadata

data ReadError = ReadIOException IOException
               | ParseError String
               | InvalidChannel
               | UnrecognizedFormat
               deriving (Show)

type Channel = Int

readStamps :: FilePath -> Channel
           -> ExceptT ReadError IO (VU.Vector Time, [Metadata])
readStamps = readStamps' formats

readStamps' :: [Format] -> FilePath -> Channel
            -> ExceptT ReadError IO (VU.Vector Time, [Metadata])
readStamps' [] _ _ = throwE UnrecognizedFormat
readStamps' (reader:rest) fname channel
  | fmtIdentify reader fname = do
      res <- lift $ runExceptT $ (fmtReader reader) fname channel
      case res of
        Left UnrecognizedFormat   -> readStamps' rest fname channel
        Left err                  -> throwE err
        Right ret                 -> return ret
  | otherwise = readStamps' rest fname channel

data Format = Format { fmtIdentify :: FilePath -> Bool
                     , fmtReader   :: FilePath -> Channel
                                   -> ExceptT ReadError IO (VU.Vector Time, [Metadata])
                     }

formats :: [Format]
formats = [fpgaTimetagger, picoharp, raw]

hasExtension :: [String] -> String -> Bool
hasExtension exts fname = any (`isSuffixOf` fname) exts

toEnum' :: (Bounded a, Enum a) => Int -> Maybe a
toEnum' n | n < minBound || n > maxBound = Nothing
          | otherwise                    = Just (toEnum n)

fpgaTimetagger :: Format
fpgaTimetagger = Format (hasExtension [".timetag"]) reader
  where
    reader fname channel = do
      ch <- maybe (throwE InvalidChannel) return $ toEnum' channel

      times <- liftIO $ withFile fname ReadMode $ \h ->
        runEffect $ PV.runToVectorP
        $ PBS.fromHandle h
        >-> Fpga.decodeRecords
        >-> Fpga.unwrapRecords
        >-> PP.filter (\(c,t) -> c == ch)
        >-> PP.map snd
        >-> PV.toVector
      mdata <- either (const []) convertMeta <$> lift (runExceptT $ Fpga.getMetadata fname)
      return (times, mdata)
    convertMeta m =
      [
        Jiffy $ 1 / realToFrac (Fpga.ttClockrate m)
      ]

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
