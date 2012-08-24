module DataSet ( Tag
               , DataSet (..)
               , dsRoot
               , getDataSets
               , hasTag
               ) where

import           Data.Aeson
import           Data.List (stripPrefix)                 
import           Data.Maybe          (fromJust, listToMaybe, mapMaybe)
import           Control.Monad       (filterM)
import           System.Directory    (doesFileExist)
import           System.FilePath     ((</>))
               
type Tag = String

data DataSet = DataSet { dsTags :: [Tag]
                       , dsFileName :: FilePath
                       }
               deriving (Show)
     
-- | Strip the given suffix from a string     
stripSuffix :: String -> String -> String
stripSuffix suffix = reverse . maybe (error "Invalid filename") id . stripPrefix (reverse suffix) . reverse

-- | The root file name of a data set
dsRoot :: DataSet -> String
dsRoot = stripSuffix ".timetag" . dsFileName

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

