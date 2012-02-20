module HPhoton.FpgaTimetagger ( Channel(..)
                              , Record(..)
                              , readRecords
                              , strobeTimes
                              , FretChannel(..)
                              , AlexChannels(..)
                              , Alex(..)
                              , alexTimes
                              ) where

import Data.Maybe (catMaybes)
import Data.Word
import Data.Bits
import HPhoton.Types
import Foreign.Storable
import Foreign.Ptr
import Data.Storable.Endian
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Storable.MMap
import Control.Monad (liftM, when)
import Control.Monad.ST
import Data.STRef

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

--instance Ord Record where
--        compare = compare `of` recTime

isDelta, isStrobe :: Record -> Bool
isDelta (DeltaRecord _ _ _ _) = True
isDelta _ = False
isStrobe (StrobeRecord _ _ _ _) = True
isStrobe _ = False

instance Storable Record where
        sizeOf _ = 6
        alignment _ = 1
        poke _ _ = error "Poke not supported"
        peek p =
             do BE a <- peek (castPtr (plusPtr p 4) :: Ptr (BigEndian Word64))
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

readRecords :: FilePath -> IO (V.Vector Record)
readRecords fname = liftM (V.drop 1024) -- Hack around hardware buffering issues
                  $ unsafeMMapVector fname Nothing
                    
-- | Fix timing wraparounds
unwrapTimes :: V.Vector Time -> V.Vector Time
unwrapTimes recs = runST (do offset <- newSTRef 0
                             lastT <- newSTRef 0
                             V.mapM (f offset lastT) recs)
        where f :: STRef s Time -> STRef s Time -> Time -> ST s Time
              f offset lastT t = do lastT' <- readSTRef lastT
                                    when (t < lastT') (modifySTRef offset (+0x1000000000))
                                    writeSTRef lastT t
                                    o <- readSTRef offset
                                    return $ t + o

strobeRecords :: V.Vector Record -> Channel -> V.Vector Record
strobeRecords recs ch = V.filter (\r->isStrobe r && ch `elem` recChannels r) recs

strobeTimes :: V.Vector Record -> Channel -> VU.Vector Time
strobeTimes recs ch = V.convert $ unwrapTimes $ V.map recTime $ strobeRecords recs ch

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
        Alex { alexAexcAem = getTimes (achAexc chs) (achAem chs)
             , alexDexcAem = getTimes (achDexc chs) (achAem chs)
             , alexAexcDem = getTimes (achAexc chs) (achDem chs)
             , alexDexcDem = getTimes (achDexc chs) (achDem chs) }
        where f :: Channel -> Channel -> ([Time], Maybe Time) -> Record -> ([Time], Maybe Time)
              f excCh _ (ts,_) (DeltaRecord {recChannels=newExcChs, recTime=t})
                | excCh `elem` newExcChs        = (ts, Just (t+offset))
                | otherwise                     = (ts, Nothing)
              f _ emCh (ts, a@(Just startT)) (StrobeRecord {recChannels=ch, recTime=t})
                | t > startT && emCh `elem` ch  = (t : ts, a)
                | otherwise                     = (ts, a)
              f _ _ (ts, Nothing) _             = (ts, Nothing)
              initial = ([], Nothing)
              getTimes excCh emCh = V.reverse $ V.fromList $ fst $ V.foldl' (f excCh emCh) initial recs


