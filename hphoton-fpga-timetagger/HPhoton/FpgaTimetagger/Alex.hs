{-# LANGUAGE PatternGuards, DeriveGeneric, FlexibleInstances #-}

module HPhoton.FpgaTimetagger.Alex ( Fret(..)
                                   , AlexChannels(..)
                                   , alexTimes
                                   ) where

import HPhoton.Types
import HPhoton.Fret (Fret(..))
import HPhoton.Fret.Alex
import HPhoton.FpgaTimetagger
import qualified Data.Vector.Unboxed as V
import Control.Monad.Trans.State.Strict
import Control.Lens
  
import qualified Data.ByteString as BS
import qualified Data.Binary as B
import Data.Vector.Binary ()
import System.FilePath
import System.Directory
import Control.Monad
                                     
data AlexChannels = AlexChannels { alexExc, alexEm :: Fret Channel }
                    deriving (Show, Eq)
                    
-- | Extract timestamps for alternating laser excitation analysis
alexTimes :: Time -> AlexChannels -> V.Vector Record -> Alex (V.Vector Time)
alexTimes offset channels recs =
  Alex { alexAexcAem = doAlex (fretA excChs) (fretA emChs)
       , alexDexcAem = doAlex (fretD excChs) (fretA emChs)
       , alexAexcDem = doAlex (fretA excChs) (fretD emChs)
       , alexDexcDem = doAlex (fretD excChs) (fretD emChs)
       }
  where doAlex exc em = getTimes offset exc em recs
        AlexChannels { alexExc=excChs, alexEm=emChs } = channels
        
data AlexState = AlexState { sAccept :: !Bool
                           , sDiscardT :: !Time
                           , sStartT :: !Time
                           , sResult :: !(V.Vector Time)
                           }
                 
getTimes :: Time -> Channel -> Channel -> V.Vector Record -> V.Vector Time
getTimes offset excCh emCh recs =
    sResult $ execState (V.mapM_ go recs) initial
  where initial = AlexState { sAccept = False
                            , sDiscardT = 0
                            , sStartT = 0
                            , sResult = V.empty
                            }
        go :: Record -> State AlexState ()
        go r = do
          state <- get
          case sAccept state of
            False | r^.recDelta && r^.recChannel excCh ->
              put $! state { sAccept = True
                           , sStartT = r^.recTime + offset
                           , sDiscardT = sDiscardT state + r^.recTime - sStartT state
                           }
            False -> return ()
            True | r^.recDelta && not (r^.recChannel excCh) -> do
              put $! state { sAccept = False
                           , sStartT = r^.recTime
                           }
            True | r^.recStrobe && r^.recTime > sStartT state && r^.recChannel emCh -> do
              put $! state { sResult = sResult state V.++ V.singleton (r^.recTime - sDiscardT state) }
            otherwise -> return ()

