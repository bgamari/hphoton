import HPhoton.Types
import HPhoton.IO.FpgaTimetagger
import HPhoton.IO.FpgaTimetagger.Alex
import Data.Function
import Data.List
import qualified Data.Vector.Unboxed as V

testDeltas = map (delta [Ch0]) [0,2000..10000]
             ++ map (delta [Ch1]) [1000,3000..10000]
  where delta chs t = buildRecord True t chs False False

testStrobes = map (strobe Ch0) [1,501..4001]
              ++ map (strobe Ch0) [4001,4101..6001]
              ++ map (strobe Ch0) [6001,6501..10001]
              
  where strobe ch t = buildRecord False t [ch] False False
        
test = V.fromList $ sortBy (compare `on` recTime) $ testDeltas ++ testStrobes
       
alexChs = AlexChannels { achAexc=Ch0
                       , achDexc=Ch1
                       , achAem = Ch0
                       , achDem = Ch1
                       }
          
main = do
  print test
  print $ alexTimes 0 alexChs test
