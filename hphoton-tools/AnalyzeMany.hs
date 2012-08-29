import           Control.Applicative ((<$>))
import           Control.Monad       ((=<<), forM_)
import           Data.Char
import           Data.Function       (on)
import           Data.List           (sortBy)
import           Data.Maybe          (fromJust, listToMaybe, mapMaybe)
import           System.Cmd
import           System.Environment  (getArgs)
import           System.FilePath     ((</>))
import           System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import           Text.Printf

import           DataSet                 

type FretEff = Double

systems = [ "frna", "rrna" ] :: [Tag]

processDirectory :: FilePath -> IO ()
processDirectory dir = do
    dss <- filter (not . (`hasTag` "ignore")) <$> getDataSets dir
    mapM_ (processDataSet dss) $ filter (`hasTag` "da") dss
    return ()

runFretAnalysis = rawSystem "fret-analysis"

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
    fit <- readFit $ dsRoot ds++"-fit.txt"
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
    hSetBuffering stdout LineBuffering
    dirs <- getArgs
    forM_ dirs $ \dir->do
        dss <- getDataSets dir
        mapM_ (processDataSet dss) $ filter (`hasTag` "da") dss

