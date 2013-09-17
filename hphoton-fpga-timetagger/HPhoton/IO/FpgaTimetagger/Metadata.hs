{-# LANGUAGE OverloadedStrings #-}

module HPhoton.IO.FpgaTimetagger.Metadata (getMetadata) where

import           Prelude hiding (readFile)

import           Control.Applicative
import           Control.Monad
import           Data.Monoid ((<>))
                 
import           Data.Aeson
import           Data.ByteString.Lazy (readFile)
import           Data.List (isSuffixOf   )
import           Data.Map (Map)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO

import           Data.Time.Format (parseTime)
import           Data.Time.LocalTime (LocalTime)
import           System.Locale (defaultTimeLocale)
       
type Freq = Int

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
    case parseTime defaultTimeLocale iso8601Format $ unpack v of
        Nothing -> mzero
        Just t  -> return t

metadataForFile f | ".meta" `isSuffixOf` f  = f
                  | otherwise               = f<>".meta"

getMetadata :: FilePath -> IO (Maybe TimetagMetadata)
getMetadata f = do
    a <- readFile $ metadataForFile f            
    return $ decode a