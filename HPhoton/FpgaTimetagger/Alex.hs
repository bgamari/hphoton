{-# LANGUAGE PatternGuards #-}

module HPhoton.FpgaTimetagger.Alex ( FretChannel(..)
                                   , AlexChannels(..)
                                   , Alex(..)
                                   , alexTimes
                                   ) where

import HPhoton.Types
import HPhoton.FpgaTimetagger
--import Data.DList
import qualified Data.Vector.Unboxed as V
import Control.Monad.Trans.State.Strict
                                     
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

