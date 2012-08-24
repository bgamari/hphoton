import           Control.Applicative ((<$>))
import           Control.Monad       (filterM, (=<<))
import           Data.Aeson
import           Data.Char
import           Data.Function       (on)
import           Data.List           (sortBy, stripPrefix, (!!))
import           Data.Maybe          (fromJust, listToMaybe, mapMaybe)
import           System.Cmd
import           System.Directory    (doesFileExist)
import           System.Environment  (getArgs)
import           System.FilePath     ((</>))
import           Text.Printf

type FretEff = Double
type Tag = String

data DataSet = DataSet { dsTags :: [Tag]
                       , dsFileName :: FilePath
                       }
               deriving (Show)

hasTag :: DataSet -> Tag -> Bool
hasTag ds tag = tag `elem` dsTags ds

systems = [ "frna", "rrna" ] :: [Tag]

getDataSets :: FilePath -> IO [DataSet]
getDataSets dir = do
    f <- readFile $ dir </> "tags"
    filterM (doesFileExist . dsFileName) $ mapMaybe parseTags $ lines f
    where parseTags line = case words line of
                                []         -> Nothing
                                file:tags  -> Just $ DataSet { dsFileName = dir </> file
                                                             , dsTags     = tags
                                                             }

processDirectory :: FilePath -> IO ()
processDirectory dir = do
    dss <- filter (not . (`hasTag` "ignore")) <$> getDataSets dir
    mapM_ (processDataSet dss) $ filter (`hasTag` "da") dss
    return ()

stripSuffix :: String -> String -> String
stripSuffix suffix = reverse . maybe (error "Invalid filename") id . stripPrefix (reverse suffix) . reverse

runFretAnalysis = rawSystem "/home/ben/lori/analysis/hphoton/cabal-dev/bin/fret-analysis"

readFit :: FilePath -> IO [(Double, Double, Double)]
readFit fitFile = do
    a <- lines <$> readFile fitFile
    return $ map (\l->let [w,mu,sigma] = words l in (read w, read mu, read sigma)) $ tail a

getDonorOnlyPeak :: DataSet -> IO FretEff
getDonorOnlyPeak ds = do
    runFretAnalysis ["--fit-ncomps=2", dsFileName ds]
    fit <- readFit $ stripSuffix ".timetag" (dsFileName ds) ++ "-fit.txt"
    let (m,mu,sigma) = last $ sortBy (compare `on` \(w,mu,sigma)->w) fit
    return mu

processDataSet :: [DataSet] -> DataSet -> IO ()
processDataSet dss ds = do
    dOnlyFret <- case findDonorOnlySets dss (fromJust $ getDataSetSystem ds) of
                 [] -> error $ "No donor only set for "++dsFileName ds
                 a:_ -> do printf "Using donor-only %s for %s\n" (dsFileName a) (dsFileName ds)
                           getDonorOnlyPeak a
    runFretAnalysis ["--crosstalk="++show dOnlyFret, dsFileName ds]
    return ()

findDonorOnlySets :: [DataSet] -> Tag -> [DataSet]
findDonorOnlySets sets tag =
    filter (not . (`hasTag` "ignore"))
    $ filter (`hasTag` tag)
    $ filter (`hasTag` "d")
    $ sets

getDataSetSystem :: DataSet -> Maybe Tag
getDataSetSystem (DataSet {dsTags=tags}) =
    listToMaybe $ filter (`elem` systems) tags

main = do
    dir:_ <- getArgs
    dss <- getDataSets dir
    mapM_ (processDataSet dss) $ filter (`hasTag` "da") dss
