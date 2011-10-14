module HPhoton.Timetag ( Channel(..)
                       , Record(..)
                       ) where

import HPhoton.Types

data Channel = C0 | C1 | C2 | C3
               deriving (Show, Eq)

data Record = DeltaRecord Stamp { recChans :: [Channels]
                                , recLost :: Bool
                        
            | StrobeRecord Time [Channels] Bool
            deriving (Show, Eq)

