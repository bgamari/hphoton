import           Control.Applicative ((<$>))
import           Control.Monad       (filterM, (=<<), forM_)
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
    return $ mapMaybe (\l->case words l of
                               "#":_        -> Nothing
                               [w,mu,sigma] -> Just (read w, read mu, read sigma)
                               otherwise    -> Nothing
                      ) a

getDonorOnlyPeak :: DataSet -> IO (Maybe FretEff)
getDonorOnlyPeak ds = do
    runFretAnalysis ["--fit-ncomps=2", dsFileName ds]
    fit <- readFit $ stripSuffix ".timetag" (dsFileName ds) ++ "-fit.txt"
    case fit of
         [] -> return Nothing
         otherwise -> let (m,mu,sigma) = last $ sortBy (compare `on` \(w,mu,sigma)->w) fit
                      in return $ Just mu

processDataSet :: [DataSet] -> DataSet -> IO ()
processDataSet dss ds = do
    dOnlyFret <- case findDonorOnlySets dss (fromJust $ getDataSetSystem ds) of
                 [] -> error $ "No donor only set for "++dsFileName ds
                 a:_ -> do printf "Using donor-only %s for %s\n"
                                  (dsFileName a) (dsFileName ds)
                           dOnly <- getDonorOnlyPeak a
                           case dOnly of
                                Nothing -> do printf "Donor only fit failed, assuming crosstalk=0\n"
                                              return 0
                                Just a  -> return a
    runFretAnalysis [ "--crosstalk="++show dOnlyFret
                    , "--burst-mode=bayes-combined"
                    , "--burst-rate=1.5"
                    , "--gamma"
                    , dsFileName ds
                    ]
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
    dirs <- getArgs
    forM_ dirs $ \dir->do
        dss <- getDataSets dir
        mapM_ (processDataSet dss) $ filter (`hasTag` "da") dss

