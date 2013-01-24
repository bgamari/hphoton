{-# LANGUAGE BangPatterns, TypeFamilies, MultiParamTypeClasses, TemplateHaskell, RankNTypes #-}

module HPhoton.FpgaTimetagger ( Channel(..)
                                -- * Timetag Record
                              , Record, emptyRecord
                              , recDelta, recStrobe
                              , recTime
                              , recChannel, recChannels
                              , recLost, recWrap
                                         
                                -- * Reading
                              , readRecords
                              , strobeTimes
                                
                                -- * Utilities
                              , getRecord
       
                              , module HPhoton.FpgaTimetagger.Metadata
                              ) where

import Data.Maybe (catMaybes)
import Data.Word
import Data.Bits
import Data.Bits.Lens
import Control.Lens
import Data.List (foldl')
import HPhoton.Types
import Foreign.Storable
import Foreign.Ptr
  
import Data.Storable.Endian
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Unboxed.Base
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Control.Monad (liftM, when)
import Control.Monad.Trans.State

import HPhoton.FpgaTimetagger.Metadata

data Channel = Ch0 | Ch1 | Ch2 | Ch3 deriving (Show, Eq, Enum, Bounded)

newtype Record = Record Word64 deriving (Eq)
makeIso ''Record
        
data RecType = Delta | Strobe deriving (Show, Eq, Enum, Bounded)
                                              
instance Show Record where
  show r = "mkRecord" 
             ++" "++views recType show r
             ++" "++views recTime show r
             ++" "++show (recChannels r)
             ++" "++views recWrap show r
             ++" "++views recLost show r
  
emptyRecord = Record 0

recDelta, recStrobe, recWrap, recLost :: Lens' Record Bool
recDelta = from record . bitAt 45
recStrobe = recDelta . iso not not
recWrap = from record . bitAt 46
recLost = from record . bitAt 47
        
recType :: Lens' Record RecType
recType = recDelta . iso to from
  where to True     = Delta
        to False    = Strobe
        from Delta  = True
        from Strobe = False
              
recTime :: Lens' Record Time
recTime = from record . lens (.&. 0xfffff)  set
  where set r t | t > 0xfffff = error "FpgaTimetagger: Time too large"
                | otherwise   = (r .&. complement 0xfffff) .|. t

recChannel :: Channel -> Lens' Record Bool
recChannel ch = from record . bitAt bit
    where bit = case ch of Ch0 -> 36
                           Ch1 -> 37
                           Ch2 -> 38
                           Ch3 -> 39

recChannels :: Record -> [Channel]
recChannels r = filter (\ch->view (recChannel ch) r) $ enumFrom minBound

derivingUnbox "Record"
    [t| Record -> Word64 |]
    [| \(Record r) -> fromIntegral r |]
    [| \r -> Record $ fromIntegral r |]

readRecords :: FilePath -> IO (Vector Record)
readRecords fname = do
    d <- BS.readFile fname
    let f bs  | BS.length bs < 6  = Nothing
              | otherwise         = case getRecord $ BS.take 6 bs of
                                        Just r  -> Just (r, BS.drop 6 bs)
                                        Nothing -> f (BS.drop 6 bs)
    return $ G.drop 1024 $ G.unfoldr f d
    
getRecord :: BS.ByteString -> Maybe Record
getRecord bs
    | BS.length bs < 6 = Nothing
    | otherwise        = Just $ Record
                         $ sum [ byte 0
                               , byte 1 `shift` 8
                               , byte 2 `shift` 16
                               , byte 3 `shift` 24
                               , byte 4 `shift` 32
                               , byte 5 `shift` 40
                               ]
    where byte i = fromIntegral $ bs `BSU.unsafeIndex` i
    
-- | Fix timing wraparounds
unwrapTimes :: Time -> Vector Time -> Vector Time
unwrapTimes maxT ts = evalState (G.mapM f ts) (0,0)
  where f :: Time -> State (Time,Time) Time
        f t = do (offset,lastT) <- get
                 let offset' = if t < lastT
                                   then offset+maxT
                                   else offset
                 put (offset', t)
                 return $! t + offset'

strobeRecords :: Vector Record -> Channel -> Vector Record
strobeRecords recs ch = G.filter (\r->r^.recStrobe && r^.recChannel ch) recs

strobeTimes :: Vector Record -> Channel -> Vector Time
strobeTimes recs ch = G.map (view recTime) $ strobeRecords recs ch

