{-# LANGUAGE PatternGuards #-}

module HPhoton.FpgaTimetagger.Alex ( FretChannel(..)
                                   , AlexChannels(..)
                                   , Alex(..)
                                   , alexTimes
                                   ) where

import HPhoton.Types
import HPhoton.FpgaTimetagger
import qualified Data.Vector.Unboxed as V
                                     
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

instance Functor Alex where
  fmap f a = Alex { alexAexcAem = f $ alexAexcAem a
                  , alexAexcDem = f $ alexAexcDem a
                  , alexDexcDem = f $ alexDexcDem a
                  , alexDexcAem = f $ alexDexcAem a
                  }

-- | Extract timestamps for alternating laser excitation analysis
alexTimes :: Time -> AlexChannels -> V.Vector Record -> Alex (V.Vector Time)
alexTimes offset chs recs =
  Alex { alexAexcAem = doAlex (achAexc chs) (achAem chs)
       , alexDexcAem = doAlex (achDexc chs) (achAem chs)
       , alexAexcDem = doAlex (achAexc chs) (achDem chs)
       , alexDexcDem = doAlex (achDexc chs) (achDem chs)
       }
  where doAlex exc em = getTimes offset exc em recs
        
getTimes :: Time -> Channel -> Channel -> V.Vector Record -> V.Vector Time
getTimes offset excCh emCh recs = V.unfoldr f (Nothing,recs)
  where f :: (Maybe Time, V.Vector Record) -> Maybe (Time, (Maybe Time, V.Vector Record))
        f (_, recs) | V.null recs  = Nothing
        f (Nothing, recs)
          | DeltaRecord {recChannels=newExcChs, recTime=t} <- V.head recs
          , excCh `elem` newExcChs  = f (Just $ t+offset, V.tail recs)
          | otherwise  = f (Nothing, V.dropWhile (not . isDelta) $ V.tail recs)
        f (Just startT, recs)
          | DeltaRecord {recChannels=newExcChs} <- V.head recs
          , excCh `notElem` newExcChs  = f (Nothing, V.tail recs)
          | StrobeRecord {recChannels=ch, recTime=t} <- V.head recs
          , t > startT, emCh `elem` ch  = Just (t-startT, (Just startT, V.tail recs))
          | otherwise  = f (Just startT, V.tail recs)
