{-# LANGUAGE BangPatterns #-}

module HPhoton.Bin ( binTimes
                   , binTimesWithRange) where

import HPhoton.Types
import Control.Monad
import Data.List

binTimes :: [Time] -> Time -> [Int]
binTimes ts width = binTimes' ts width Nothing ((head ts) `quot` width) 0

binTimesWithRange :: [Time] -> Time -> (Time, Time) -> [Int]
binTimesWithRange ts width (start_t, end_t) = binTimes' ts width (Just end_t) (start_t `quot` width) 0

binTimes' :: [Time] -> Time -> Maybe Time -> Int -> Int -> [Int]
binTimes' (t:ts) width end_t !bin_n !count
        -- Make sure the current time is greater than start_t
        | t < bin_n*width       = binTimes' ts width end_t bin_n count
        -- Make sure the current time isn't past end of the current bin
        | t >= (bin_n+1)*width  = let
                                  rest = binTimes' (t:ts) width end_t (bin_n+1) 0 
                                  in
                                  count : rest
        -- The photon is in our bin, increment count
        | otherwise             = binTimes' ts width end_t bin_n (count+1)

binTimes' [] width (Just end_t) !bin_n !count
        | end_t > bin_n*width   = let
                                  rest = binTimes' [] width (Just end_t) (bin_n+1) 0
                                  in
                                  count : rest
        | otherwise             = []

binTimes' [] width Nothing bin_n count = []

testTimes :: [Time]
testTimes = [5*i | i <- [0..10*1000*1000]]

main :: IO ()
main = do
        let binned = binTimes testTimes 10
        forM_ binned $ \count -> print count

