{-# LANGUAGE PatternGuards, DeriveGeneric, FlexibleInstances #-}

module HPhoton.FpgaTimetagger.Alex ( FretChannel(..)
                                   , AlexChannels(..)
                                   , alexTimes
                                     
                                     -- * ALEX cache
                                   , getCachedAlex
                                   , putCachedAlex
                                   ) where

import HPhoton.Types
import HPhoton.Fret.Alex
import HPhoton.FpgaTimetagger
import qualified Data.Vector.Unboxed as V
import Control.Monad.Trans.State.Strict
  
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import System.FilePath
import System.Directory
import Control.Monad
                                     
data FretChannel = Acceptor | Donor deriving (Show, Eq)

data AlexChannels = AlexChannels { achAexc :: Channel
                                 , achDexc :: Channel
                                 , achAem :: Channel
                                 , achDem :: Channel
                                 } deriving (Show, Eq)

instance (V.Unbox a, S.Serialize a) => S.Serialize (V.Vector a) where
  put = S.put . V.toList
  get = V.fromList `liftM` S.get
                              
-- | Extract timestamps for alternating laser excitation analysis
alexTimes :: Time -> AlexChannels -> V.Vector Record -> Alex (V.Vector Time)
alexTimes offset chs recs =
  Alex { alexAexcAem = doAlex (achAexc chs) (achAem chs)
       , alexDexcAem = doAlex (achDexc chs) (achAem chs)
       , alexAexcDem = doAlex (achAexc chs) (achDem chs)
       , alexDexcDem = doAlex (achDexc chs) (achDem chs)
       }
  where doAlex exc em = getTimes offset exc em recs
        
data AlexState = AlexState { sAccept :: !Bool
                           , sDiscardT :: !Time
                           , sStartT :: !Time
                           , sResult :: V.Vector Time
                           }
                 
getTimes :: Time -> Channel -> Channel -> V.Vector Record -> V.Vector Time
getTimes offset excCh emCh recs =
  sResult $ execState (V.mapM_ f recs) initial
  where initial = AlexState { sAccept = False
                            , sDiscardT = 0
                            , sStartT = 0
                            , sResult = V.empty
                            }
        f :: Record -> State AlexState ()
        f rec = do
          state <- get
          case sAccept state of
            False | recDelta rec && recChannel rec excCh ->
              put $! state { sAccept = True
                           , sStartT = recTime rec + offset
                           , sDiscardT = sDiscardT state + recTime rec - sStartT state
                           }
            False -> return ()
            True | recDelta rec && not (recChannel rec excCh) -> do
              put $! state { sAccept = False
                           , sStartT = recTime rec
                           }
            True | recStrobe rec && recTime rec > sStartT state && recChannel rec emCh -> do
              put $! state { sResult = sResult state V.++ V.singleton (recTime rec - sDiscardT state) }
            otherwise -> return ()

cachedAlexPath fname =
  let cacheName = "."++takeFileName fname++".alex"
  in  dropFileName fname </> cacheName
                  
getCachedAlex :: FilePath -> IO (Maybe (Alex (V.Vector Time)))
getCachedAlex fname = do
  exists <- doesFileExist cachePath
  if exists
    then do a <- S.decode `liftM` BS.readFile cachePath
            return $ case a of
              Left _  -> Nothing
              Right a -> Just a
    else return Nothing
  where cachePath = cachedAlexPath fname
  
putCachedAlex :: FilePath -> Alex (V.Vector Time) -> IO ()
putCachedAlex fname alex = 
  BS.writeFile cachePath $ S.encode alex
  where cachePath = cachedAlexPath fname
  
