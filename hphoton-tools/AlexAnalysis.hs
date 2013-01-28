import           Control.Lens hiding ((^=))
import           Data.Accessor
import qualified Data.Foldable as F
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           System.IO

import           Control.Proxy as P
import qualified Control.Proxy.ByteString as PBS
import           Control.Proxy.Vector

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU

import           HPhoton.Types
import           HPhoton.FpgaTimetagger.Pipe
import           HPhoton.FpgaTimetagger.Alex
import           HPhoton.Bin.Alex
import           HPhoton.Fret.Alex

import           Graphics.Rendering.Chart
import           Data.Colour
import           Data.Colour.Names

import           Numeric.SpecFunctions (factorial)

type Rate = Double

poissonP :: Rate -> Int -> Double
poissonP l k = l^k / factorial k * exp (-l)

bgOdds :: Rate -> Rate -> Int -> Double
bgOdds bg fg k = poissonP fg k / poissonP bg k

main = do
    let fname = "/home/ben/lori/data/rna/solution-fret/2013-01-21/2013-01-21-run_004.timetag"
    recs <- withFile fname ReadMode $ \fIn->
        runToVectorD $ runProxy $   raiseK (PBS.fromHandleS fIn)
                                >-> decodeRecordsP
                                >-> dropD 1024
                                >-> filterDeltasP
                                -- >-> takeB 100000
                                >-> toVectorD
    
    let alexChannels = AlexChannels { alexExc = Fret Ch1 Ch0
                                    , alexEm  = Fret Ch1 Ch0
                                    }
    let clk = clockFromFreq $ round (128e6::Double)
    let times = alexTimes 0 alexChannels recs
        thresh = Alex { alexAexcAem = 5, alexAexcDem = 0
                      , alexDexcAem = 5, alexDexcDem = 0 }
        bins = fmap (fmap realToFrac)
               $ filter (\alex->getAll $ F.fold
                                $ pure (\a b->All $ a > b) <*> alex <*> thresh)
               $ alexBin (realTimeToTime clk 10e-3) times

    writeFile "test" $ unlines $ map (F.foldMap (\a->show a++"\t")) bins
    let s = fmap stoiciometry bins
        e = fmap proxRatio bins

    let chart = layout1_plots ^= [Left $ toPlot pts]
                $ defaultLayout1
        pts = plot_points_values ^= zip e s
              $ plot_points_style ^= filledCircles 2 (opaque blue)
              $ defaultPlotPoints
    renderableToPDFFile (toRenderable chart) 640 480 "test.pdf"
    