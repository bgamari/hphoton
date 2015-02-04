module FretFit ( fitFret ) where

import           Prelude hiding (catch)
import           Control.Exception (SomeException, catch)
import           Control.Monad

import           Data.Random hiding (Gamma, gamma)
import qualified Data.Vector.Generic as V
import           Numeric.MixtureModel.Beta as Beta hiding (Prob)
import           System.Random.MWC

import           HPhoton.Fret

priors :: Int -> [(Weight, BetaParam)]
priors ncomps = map component [1..ncomps]
  where
    component i = (weight, param)
      where
        weight = 1 / realToFrac ncomps
        Just param = paramFromMoments (realToFrac i / realToFrac (ncomps+2)) 0.01

runFit :: Int -> Int -> Samples -> RVar ComponentParams
runFit ncomps niter samples = do
    a0 <- updateAssignments' samples (V.fromList $ priors ncomps)
    a <- foldM (const . updateAssignments samples ncomps) a0 [1..500]
    return $ estimateWeights a $ paramsFromAssignments samples ncomps a

fitFailed :: SomeException -> IO (Maybe ComponentParams)
fitFailed _ = return Nothing

fitFret :: Int -> Int -> [FretEff] -> IO (Maybe ComponentParams)
fitFret niter ncomps fretEffs =
    withSystemRandom $ \mwc->catch (Just `liftM` go mwc) fitFailed
  where
    go = runRVar $ runFit ncomps niter $ V.fromList $ filter (\x->x>0 && x<1) fretEffs
