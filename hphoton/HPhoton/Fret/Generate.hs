module HPhoton.Fret.Generate where
       
import HPhoton.Types
import HPhoton.Fret.Types

import Control.Applicative
import Data.Traversable as T

import Data.Random
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Poisson
       
fretBin :: Beta Double -> Double -> RVar (Fret Int)
fretBin fretEff rate = do
    n <- integralPoisson rate
    e <- sample fretEff
    nA <- integralBinomial n e
    return $ Fret nA (n-nA)

addBg :: Fret Double -> Fret Int -> RVar (Fret Int)
addBg rates n = do
    nBg <- T.mapM integralPoisson rates
    return $ (+) <$> nBg <*> n