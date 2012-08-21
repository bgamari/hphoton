{-# LANGUAGE BangPatterns, TypeFamilies, MultiParamTypeClasses #-}

module HPhoton.FpgaTimetagger ( Channel(..)
                                -- * Timetag Record
                              , Record
                              , recDelta, recStrobe
                              , recTime
                              , recChannel, recChannels
                              , recLost, recWrap
                                         
                                -- * Reading
                              , readRecords
                              , strobeTimes
                                
                                -- * Utilities
                              , buildRecord
       
                              , module HPhoton.FpgaTimetagger.Metadata
                              ) where

import Data.Maybe (catMaybes)
import Data.Word
import Data.Bits
import Data.List (foldl')
import HPhoton.Types
import Foreign.Storable
import Foreign.Ptr
  
import Data.Storable.Endian
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Unboxed.Base
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.ByteString as BS

import Control.Monad (liftM, when)
import Control.Monad.Trans.State

import HPhoton.FpgaTimetagger.Metadata

data Channel = Ch0 | Ch1 | Ch2 | Ch3 deriving (Show, Eq, Enum, Bounded)

newtype Record = Record (Time,Word8) deriving (Eq)
                                              
recTime (Record (time,_)) = time
recFlags (Record (_,flags)) = flags
                     
instance Show Record where
  show rec = "buildRecord" 
             ++" "++show (recDelta rec)
             ++" "++show (recTime rec)
             ++" "++show (recChannels rec)
             ++" "++show (recWrap rec)
             ++" "++show (recLost rec)
  
buildRecord :: Bool -> Word64 -> [Channel] -> Bool -> Bool -> Record
buildRecord isDelta time channels isWrap isLost = Record (time,flags)
  where flags = foldl' (.|.) 0 $ 
                [ flag bitDelta isDelta 
                , flag bitWrap isWrap
                , flag bitLost isLost
                ] ++ map (bit . bitCh) channels
        flag _ False = 0
        flag a True  = bit a
                         
recDelta, recStrobe, recWrap, recLost :: Record -> Bool
recDelta rec = recFlags rec `testBit` bitDelta
recStrobe = not . recDelta
recWrap rec = recFlags rec `testBit` bitWrap
recLost rec = recFlags rec `testBit` bitLost
              
recChannel :: Record -> Channel -> Bool
recChannel rec ch = recFlags rec `testBit` bitCh ch

recChannels :: Record -> [Channel]
recChannels rec = filter (recChannel rec) $ enumFrom minBound

bitDelta = 1
bitLost = 2
bitWrap = 3
bitCh Ch0 = 4
bitCh Ch1 = 5
bitCh Ch2 = 6
bitCh Ch3 = 7

boolToList a False = []
boolToList a True = [a]
                    
pack :: Record -> (Time, Word8)
pack (Record a) = a
        
unpack :: (Time, Word8) -> Record
unpack a = Record a
        
newtype instance MVector s Record = MV_Record (MVector s (Time, Word8))
newtype instance Vector    Record = V_Record  (Vector    (Time, Word8))
instance Unbox Record

instance M.MVector MVector Record where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Record v) = M.basicLength v
  basicUnsafeSlice i n (MV_Record v) = MV_Record $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Record v1) (MV_Record v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Record `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n rec = MV_Record `liftM` M.basicUnsafeReplicate n (pack rec)
  basicUnsafeRead (MV_Record v) i = unpack `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Record v) i rec = M.basicUnsafeWrite v i $ pack rec
  basicClear (MV_Record v) = M.basicClear v
  basicSet (MV_Record v) rec = M.basicSet v $ pack rec
  basicUnsafeCopy (MV_Record v1) (MV_Record v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Record v1) (MV_Record v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Record v) n = MV_Record `liftM` M.basicUnsafeGrow v n

instance G.Vector Vector Record where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Record v) = V_Record `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Record v) = MV_Record `liftM` G.basicUnsafeThaw v
  basicLength (V_Record v) = G.basicLength v
  basicUnsafeSlice i n (V_Record v) = V_Record $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Record v) i = unpack `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Record mv) (V_Record v) = G.basicUnsafeCopy mv v
  elemseq _ rec y = G.elemseq (undefined :: Vector (Time,Word8)) (pack rec) y


--instance Ord Record where
--        compare = compare `of` recTime

-- | Get a record
getRecord :: BS.ByteString -> Record
getRecord bs =
  let [flags1, flags2, stamp4, stamp3, stamp2, stamp1] = BS.unpack bs
      time =    ((fromIntegral $ flags2 .&. 0xf) `unsafeShiftL` 32)
             .|. (fromIntegral stamp4 `unsafeShiftL` 24)
             .|. (fromIntegral stamp3 `unsafeShiftL` 16)
             .|. (fromIntegral stamp2 `unsafeShiftL`  8)
             .|. (fromIntegral stamp1 `unsafeShiftL`  0)
      chs = concatMap (\(ch,bit)->boolToList ch $ testBit flags2 bit)
            [(Ch0,4), (Ch1,5), (Ch2,6), (Ch3,7)]
      delta = flags1 `testBit` 5
      wrap =  flags1 `testBit` 6
      lost =  flags1 `testBit` 7
  in buildRecord delta time chs wrap lost

readRecords :: FilePath -> IO (Vector Record)
readRecords fname = do
  d <- BS.readFile fname
  let f bs  | BS.length bs < 6  = Nothing
            | otherwise         = Just (getRecord $ BS.take 6 bs, BS.drop 6 bs)
  return $ unwrapTimes $ G.drop 1024 $ G.unfoldr f d
    
-- | Fix timing wraparounds
unwrapTimes :: Vector Record -> Vector Record
unwrapTimes recs = evalState (G.mapM f recs) (0,0)
  where f :: Record -> State (Time,Time) Record
        f (Record (t,flags)) = do
          (offset,lastT) <- get
          let offset' = if t < lastT
                           then offset+0x1000000000
                           else offset
          put (offset', t)
          return $! Record (t + offset', flags)

strobeRecords :: Vector Record -> Channel -> Vector Record
strobeRecords recs ch = G.filter (\r->recStrobe r && ch `elem` recChannels r) recs

strobeTimes :: Vector Record -> Channel -> Vector Time
strobeTimes recs ch = G.map recTime $ strobeRecords recs ch

main = do
  recs <- readRecords "hi2.timetag"
  G.mapM_ print $ G.take 100 recs
