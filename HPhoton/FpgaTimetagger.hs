module HPhoton.FpgaTimetagger ( Channel(..)
                              , Record(..)
                              , readRecords
                              , strobeChTimes
                              , FretChannel
                              , AlexChannels
                              , Alex
                              , alexTimes
                              ) where

import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Binary
import Data.Binary.Get (runGet, isEmpty, getWord32be, getWord16be)
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.Bits
import HPhoton.Types

data Channel = Ch0 | Ch1 | Ch2 | Ch3
               deriving (Show, Eq)

data Record = DeltaRecord { recTime :: Time
                          , recChannels :: [Channel]
                          , recWrap :: Bool
                          , recLost :: Bool
                          }
            | StrobeRecord { recTime :: Time
                           , recChannels :: [Channel]
                           , recWrap :: Bool
                           , recLost :: Bool
                           }
            deriving (Show, Eq)

isDelta, isStrobe :: Record -> Bool
isDelta (DeltaRecord _ _ _ _) = True
isDelta _ = False
isStrobe (StrobeRecord _ _ _ _) = True
isStrobe _ = False

getWord48be :: Get Word64
getWord48be = do b <- getWord16be
                 a <- getWord32be
                 return $ fromIntegral a .|. (fromIntegral b  `shift` 32)

instance Binary Record where
        put = undefined
        get = do a <- getWord48be
                 let time = a .&. 0xfffffffff
                     chToMaybe ch False = Nothing
                     chToMaybe ch True = Just ch
                     chs = catMaybes [ chToMaybe Ch0 $ testBit a 36
                                     , chToMaybe Ch1 $ testBit a 37
                                     , chToMaybe Ch2 $ testBit a 38
                                     , chToMaybe Ch3 $ testBit a 39 ]
                     rtype = testBit a 45
                     wrap = testBit a 46
                     lost = testBit a 47
                 if rtype then return DeltaRecord { recTime = time
                                                  , recChannels = chs
                                                  , recWrap = wrap
                                                  , recLost = lost
                                                  }
                         else return StrobeRecord { recTime = time
                                                  , recChannels = chs
                                                  , recWrap = wrap
                                                  , recLost = lost
                                                  }

-- | Gets all entries
many :: Binary a => Get a -> Get [a]
many g = liftM reverse $ f []
        where f l = do e <- isEmpty
                       if e then return l
                            else do a <- g
                                    f (a:l)

readRecords :: FilePath -> IO [Record]
readRecords fname = do a <- B.readFile fname
                       return $ runGet (many (get::Get Record)) a

strobeChTimes :: [Record] -> Channel -> [Time]
strobeChTimes recs ch = map recTime $ filter (\r->isStrobe r && ch `elem` recChannels r) recs

data FretChannel = Acceptor | Donor deriving (Show, Eq)

data AlexChannels = AlexChannels { achAexc :: Channel
                                 , achDexc :: Channel
                                 , achAem :: Channel
                                 , achDem :: Channel
                                 } deriving (Show, Eq)

data Alex a = Alex { alexAexcAem :: a
                   , alexAexcDem :: a
                   , alexDexcAem :: a
                   , alexDexcDem :: a
                   } deriving (Show, Eq)

-- | Extract timestamps for alternating laser excitation analysis
alexTimes :: Time -> AlexChannels -> [Record] -> Alex [Time]
alexTimes offset chs = fst . foldl' f (Alex [] [] [] [], Nothing)
        where f :: (Alex [Time], Maybe (Time,FretChannel)) -> Record -> (Alex [Time], Maybe (Time,FretChannel))
              f (ts,_) (DeltaRecord {recChannels=newExcChs, recTime=t})
                | achAexc chs `elem` newExcChs  = (ts, Just (t+offset, Acceptor))
                | achDexc chs `elem` newExcChs  = (ts, Just (t+offset, Donor))
                | otherwise                     = (ts, Nothing)
              f (ts, a@(Just (startT, excCh))) (StrobeRecord {recChannels=ch, recTime=t})
                | t > startT = case excCh of 
                        Acceptor | achAem chs `elem` ch   -> (ts {alexAexcAem=t : alexAexcAem ts}, a)
                        Acceptor | achDem chs `elem` ch   -> (ts {alexAexcDem=t : alexAexcDem ts}, a)
                        Donor    | achAem chs `elem` ch   -> (ts {alexDexcAem=t : alexDexcAem ts}, a)
                        Donor    | achDem chs `elem` ch   -> (ts {alexDexcDem=t : alexDexcDem ts}, a)
                                 | otherwise              -> (ts, a)
                | otherwise = (ts, a)

