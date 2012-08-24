module DataSet ( Tag
               , DataSet (..)
               , getDataSets
               , hasTag
               ) where

import           System.FilePath     ((</>))
import           System.Directory    (doesFileExist)
import           Data.Maybe          (fromJust, listToMaybe, mapMaybe)
import           Data.Aeson
import           Control.Monad       (filterM)
               
type Tag = String

data DataSet = DataSet { dsTags :: [Tag]
                       , dsFileName :: FilePath
                       }
               deriving (Show)

-- | Test whether a data set is tagged with the given tag
hasTag :: DataSet -> Tag -> Bool
hasTag ds tag = tag `elem` dsTags ds

-- | Read tags in directory       
getDataSets :: FilePath -> IO [DataSet]
getDataSets dir = do
    f <- readFile $ dir </> "tags"
    filterM (doesFileExist . dsFileName) $ mapMaybe parseTags $ lines f
    where parseTags line = case words line of
                                []         -> Nothing
                                file:tags  -> Just $ DataSet { dsFileName = dir </> file
                                                             , dsTags     = tags
                                                             }

