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
        
data AlexState = AlexState { sAccept :: !Bool
                           , sDiscardT :: !Time
                           , sStartT :: !Time
                           , sRecords :: V.Vector Record
                           }
                 
getTimes :: Time -> Channel -> Channel -> V.Vector Record -> V.Vector Time
getTimes offset excCh emCh recs =
  V.unfoldr f $ AlexState {sAccept=False, sDiscardT=0, sStartT=0, sRecords=recs}
  where f :: AlexState -> Maybe (Time, AlexState)
        f state | V.null $ sRecords state  = Nothing
        f state@(AlexState {sAccept=False, sRecords=recs})
          | rec <- V.head recs, recDelta rec, recChannel rec excCh =
              f $ state { sAccept=True
                        , sStartT=recTime rec + offset
                        , sDiscardT=sDiscardT state + recTime rec - sStartT state
                        , sRecords=V.tail recs
                        }
          | otherwise  =
              f $ state { sRecords=V.dropWhile (not . recDelta) $ V.tail recs }
        f state@(AlexState {sAccept=True, sRecords=recs})
          | rec <- V.head recs, recDelta rec, not $ recChannel rec excCh =
              f $ state { sAccept=False, sRecords=V.tail recs }
          | rec <- V.head recs, recTime rec > sStartT state, recChannel rec emCh =
              Just (recTime rec - sDiscardT state, state {sRecords=V.tail recs})
          | otherwise  =
              f $ state {sRecords=V.tail recs}
