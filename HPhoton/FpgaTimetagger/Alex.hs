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
        Alex { alexAexcAem = V.convert $ getTimes (achAexc chs) (achAem chs)
             , alexDexcAem = V.convert $ getTimes (achDexc chs) (achAem chs)
             , alexAexcDem = V.convert $ getTimes (achAexc chs) (achDem chs)
             , alexDexcDem = V.convert $ getTimes (achDexc chs) (achDem chs) }
        where f :: Channel -> Channel -> ([Time], Maybe Time) -> Record -> ([Time], Maybe Time)
              f excCh _ (ts,_) (DeltaRecord {recChannels=newExcChs, recTime=t})
                | excCh `elem` newExcChs        = (ts, Just (t+offset))
                | otherwise                     = (ts, Nothing)
              f _ emCh (ts, a@(Just startT)) (StrobeRecord {recChannels=ch, recTime=t})
                | t > startT && emCh `elem` ch  = (t : ts, a)
                | otherwise                     = (ts, a)
              f _ _ (ts, Nothing) _             = (ts, Nothing)
              initial = ([], Nothing)
              getTimes excCh emCh = V.reverse $ V.fromList $ fst
                                    $ V.foldl' (f excCh emCh) initial recs

