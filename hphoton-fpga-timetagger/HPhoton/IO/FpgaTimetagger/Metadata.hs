{-# LANGUAGE OverloadedStrings #-}

module HPhoton.IO.FpgaTimetagger.Metadata
  ( getMetadata
  , TimetagMetadata(..)
  ) where

import           Prelude hiding (readFile)

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Monoid ((<>))

import           Data.Aeson
import           Data.ByteString.Lazy (readFile)
import           Data.List (isSuffixOf   )
import           Data.Map (Map)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO

import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import           Data.Time.LocalTime (LocalTime)

type Freq = Int  -- ^ in Hertz

data TimetagMetadata = TimetagMeta { ttDescription :: Text
                                   , ttInstrument :: Text
                                   , ttHardwareVer :: Text
                                   , ttChannels :: Map Text Text
                                   , ttStartTime :: LocalTime
                                   , ttClockrate :: Freq
                                   }
                       deriving (Show, Eq)

instance FromJSON TimetagMetadata where
  parseJSON (Object v) = TimetagMeta <$>
                         v .: "description" <*>
                         v .: "instrument" <*>
                         v .: "hardware version" <*>
                         v .: "channels" <*>
                         (readIso8601 =<< (v .: "start")) <*>
                         v .: "clockrate"
  parseJSON _ = mzero

iso8601Format = "%FT%T%Q"

readIso8601 :: MonadPlus m => Text -> m LocalTime
readIso8601 v =
    case parseTimeM True defaultTimeLocale iso8601Format (unpack v) of
        Nothing -> mzero
        Just t  -> return t

metadataForFile f | ".meta" `isSuffixOf` f  = f
                  | otherwise               = f<>".meta"

getMetadata :: FilePath -> EitherT String IO TimetagMetadata
getMetadata f = do
    a <- fmapLT show $ tryIO $ readFile $ metadataForFile f
    hoistEither $ eitherDecode a
