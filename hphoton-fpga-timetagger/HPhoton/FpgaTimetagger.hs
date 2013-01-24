{-# LANGUAGE BangPatterns, TypeFamilies, MultiParamTypeClasses, TemplateHaskell, RankNTypes #-}

module HPhoton.FpgaTimetagger ( -- * Types and fields
                                Record, zeroRecord
                              , recDelta, recStrobe
                              , recTime
                              , Channel(..)
                              , recChannel, recChannels
                              , recLost, recWrap
                                         
                                -- * Reading records
                              , readRecords
                              , decodeRecords
                              , strobeTimes
                                
                                -- * Metadata
                              , module HPhoton.FpgaTimetagger.Metadata
                              ) where

import           Data.Maybe (catMaybes)
import           Data.Word
import           Data.Bits
import           Data.Bits.Lens
import           Control.Lens
import           Data.List (foldl')
import           HPhoton.Types
import           Foreign.Storable
import           Foreign.Ptr
  
import           Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Storable.Endian
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import           Data.Vector.Unboxed.Base
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable as VM

import           Control.Monad (liftM, when)
import           Control.Monad.Trans.State

import           HPhoton.FpgaTimetagger.Metadata

-- | A timetagger input channel
data Channel = Ch0 | Ch1 | Ch2 | Ch3 deriving (Show, Eq, Enum, Bounded)

-- | A timetagger event record
newtype Record = Record Word64 deriving (Eq)
makeIso ''Record
        
instance Show Record where
  show r = "mkRecord" 
             ++" "++views recType show r
             ++" "++views recTime show r
             ++" "++show (recChannels r)
             ++" "++views recWrap show r
             ++" "++views recLost show r
  
zeroRecord = Record 0

-- | Is the record a delta record
recDelta :: Lens' Record Bool
recDelta = from record . bitAt 45

-- | Is the record a strobe record
recStrobe :: Lens' Record Bool
recStrobe = recDelta . iso not not

-- | The type of a record
data RecType = Delta | Strobe deriving (Show, Eq, Enum, Bounded)
                                              
-- | The type of a record
recType :: Lens' Record RecType
recType = recDelta . iso to from
  where to True     = Delta
        to False    = Strobe
        from Delta  = True
        from Strobe = False
              
-- | The wrap bit of a record
recWrap :: Lens' Record Bool
recWrap = from record . bitAt 46

-- | The "records lost" bit of a record
recLost :: Lens' Record Bool
recLost = from record . bitAt 47

-- | The time field of a record
recTime :: Lens' Record Time
recTime = from record . lens (.&. mask)  set
  where set r t | t > mask   = error "FpgaTimetagger: Time too large"
                | otherwise  = (r .&. complement mask) .|. t
        mask = 0xfffffffff

-- | Is the given channel's bit set
recChannel :: Channel -> Lens' Record Bool
recChannel ch = from record . bitAt bit
    where bit = case ch of Ch0 -> 36
                           Ch1 -> 37
                           Ch2 -> 38
                           Ch3 -> 39

-- | All of the channel bits set in a record
recChannels :: Record -> [Channel]
recChannels r = filter (\ch->view (recChannel ch) r) $ enumFrom minBound

derivingUnbox "Record"
    [t| Record -> Word64 |]
    [| \(Record r) -> fromIntegral r |]
    [| \r -> Record $ fromIntegral r |]

-- | Read records from a file
-- Note that this will drop the first 1024 records
-- as a precaution against records leaking from stale buffers
readRecords :: FilePath -> IO (Vector Record)
readRecords fname = G.drop 1024 . decodeRecords <$> BS.readFile fname
    
-- | Decode records from a bytestring
decodeRecords :: BS.ByteString -> Vector Record
decodeRecords = G.unfoldr f
    where f bs | BS.length bs < 6  = Nothing
               | otherwise         = case decodeRecord $ BS.take 6 bs of
                                         Just r  -> Just (r, BS.drop 6 bs)
                                         Nothing -> f (BS.drop 6 bs)
    
-- | Decode a single record from a bytestring
decodeRecord :: BS.ByteString -> Maybe Record
decodeRecord bs
    | BS.length bs < 6 = Nothing
    | otherwise        = Just $ Record
                         $ sum [ byte 5
                               , byte 4 `shift` 8
                               , byte 3 `shift` 16
                               , byte 2 `shift` 24
                               , byte 1 `shift` 32
                               , byte 0 `shift` 40
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

-- | Return all of the strobe records for a given channel
strobeRecords :: Vector Record -> Channel -> Vector Record
strobeRecords recs ch = G.filter (\r->r^.recStrobe && r^.recChannel ch) recs

-- | Return all of the strobe event times for a given channel
strobeTimes :: Vector Record -> Channel -> Vector Time
strobeTimes recs ch = G.map (view recTime) $ strobeRecords recs ch

