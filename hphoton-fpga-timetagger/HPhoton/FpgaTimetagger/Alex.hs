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
        
getTimes :: Time -> Channel -> Channel -> V.Vector Record -> V.Vector Record
getTimes offsetT excCh emCh recs =
    evalState (V.filterM go recs) Nothing
  where go :: Record -> State (Maybe Time) Bool
        go r = do
          accept <- get
          case accept of
            Nothing | r^.recDelta && r^.recChannel excCh -> do
                put $! Just $ (r^.recTime + offsetT) .&. recTimeMask
                return False

            Just _ | r^.recDelta && not (r^.recChannel excCh) -> do
                put Nothing
                return False

            Just startT | r^.recStrobe && r^.recTime > startT && r^.recChannel emCh ->
                return True

            otherwise ->
                return False
