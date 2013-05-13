{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

import           Control.Monad
import           Control.Applicative
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V

import           Text.Printf
import           System.Console.CmdArgs

import           Data.Colour
import           Data.Colour.Names
import           Control.Lens
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Plot.Histogram
import           Numeric.Log hiding (sum)

import           Numeric.MixtureModel.Exponential
import           HPhoton.FpgaTimetagger
import           HPhoton.Bin.Plot
import           HPhoton.Utils
import           HPhoton.Types hiding (Freq)

type Freq = Double

data PlotArgs = PlotArgs { file             :: FilePath
                         , model            :: Maybe FilePath
                         , output           :: FilePath
                         , channel          :: Int
                         , clockrate        :: Freq
                         , short_cutoff     :: RealTime
                         }
              deriving (Data, Typeable, Show)

argsChannel :: PlotArgs -> Channel
argsChannel (PlotArgs {channel=ch}) =
    case ch of
        0         -> Ch0
        1         -> Ch1
        2         -> Ch2
        3         -> Ch3
        otherwise -> error "Invalid channel"

plotArgs = PlotArgs
    { file = "" &= typFile &= argPos 0
    , model = Nothing &= typFile
    , channel = 0 &= groupname "Input"
                  &= help "Channel to fit"
    , clockrate = 128e6 &= groupname "Input"
                        &= typ "FREQ"
                        &= help "Instrument clockrate (default=128 MHz)"
    , short_cutoff = 1e-6 &= typ "TIME"
                          &= groupname "Input"
                          &= help "Discard interarrival times smaller than TIME (default=1 us)"
    , output = "" &= typFile
    }

longTime = 5e-2

histPlot :: (Double, Double) -> V.Vector Sample -> Plot Sample Double
histPlot range xs = histToPlot
              $ plot_hist_bins     .~ 4000
              $ plot_hist_values   .~ V.convert xs
              $ plot_hist_range    .~ Just range
              $ plot_hist_no_zeros .~ True
              $ defaultNormedPlotHist

functionPlot :: (RealFrac x, Enum x) => Int -> (x, x) -> (x -> y) -> Plot x y
functionPlot n (a,b) f =
  let xs = [a,a+(b-a)/realToFrac n..b]
  in toPlot $ plot_lines_values .~ [map (\x->(x,f x)) xs]
            $ plot_lines_style .> line_color .~ opaque red
            $ defaultPlotLines

plotFit :: V.Vector Sample -> (Double,Double) -> [Double->Double] -> Layout1 Double Double
plotFit samples (a,b) fits =
    layout1_plots .~ [ Left $ histPlot (a,b) samples ]
                     ++ map (Left . functionPlot 1000 (a,b)) fits
    $ layout1_left_axis . laxis_generate .~ autoScaledLogAxis defaultLogAxis
    $ defaultLayout1

plotParamSample :: V.Vector Sample -> Maybe ComponentParams -> IO ()
plotParamSample samples paramSample = do
  let fits = case paramSample of
                  Nothing -> []
                  Just s -> let dist x = sum $ map (\(w,p)->w * realToFrac (prob p x)) $ VB.toList s
                            in [dist]
  renderableToPDFFile (toRenderable $ plotFit samples (1e-7,longTime) fits)
                      640 480 "all.pdf"

main = do
    pargs <- cmdArgs plotArgs
    recs <- readRecords $ file pargs
    let jiffy = 1 / clockrate pargs
        samples = V.filter (>short_cutoff pargs)
                  $ V.map ((jiffy*) . realToFrac)
                  $ timesToInterarrivals
                  $ strobeTimes recs (argsChannel pargs)
               :: V.Vector Sample

    params <- case model pargs of
        Nothing -> return Nothing
        Just f  -> (Just . VB.fromList . read) <$> readFile f

    --let times = strobeTimes recs (argsChannel pargs)
    --renderableToPDFFile (plotRecords times params) 1000 500 "hello.pdf"

    plotParamSample samples params

plotRecords :: V.Vector Time -> ComponentParams -> Renderable ()
plotRecords times params = renderLayout1sStacked
    [ withAnyOrdinate
      $ layout1_plots .~ [Left bins]
      $ defaultLayout1
    , withAnyOrdinate
      $ layout1_plots .~ [ Left $ photons (\odds->odds > 0 && odds < 2) green
                         , Left $ photons (\odds->odds > 1 && odds < 2) blue
                         , Left $ photons (\odds->odds > 2) red
                         ]
      $ defaultLayout1
    ]
    where clk = clockFromFreq (round 128e6)
          bins = plotBins clk times 1e-2 "Donor" green
          (bgWeight, bgParams) = params VB.! 0
          (flWeight, flParams) = params VB.! 1
          odds :: RealTime -> Prob
          odds dt = (realToFrac flWeight * prob flParams dt)
                  / (realToFrac bgWeight * prob bgParams dt)
          photons cutoff color = toPlot
                    $ plot_points_values .~ (V.toList
                        -- $ V.filter (\(_,odds)->cutoff odds)
                        $ V.map (\(t,odds)->(t, ln odds))
                        $ V.map (\(t,dt)->( timeToRealTime clk t
                                          , odds $ timeToRealTime clk dt))
                        $ V.zip times (timesToInterarrivals times)
                        )
                    $ plot_points_style .~ plusses 1 0.1 (opaque color)
                    $ defaultPlotPoints
                    :: Plot RealTime Double
