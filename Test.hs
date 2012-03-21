module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified HPhoton.BurstIdent.Bayes
import qualified HPhoton.Bin
import qualified HPhoton.Utils
  
tests = [ testGroup "BurstIdent.Bayes" HPhoton.BurstIdent.Bayes.tests
        , testGroup "Bin" HPhoton.Bin.tests
        , testGroup "Utils" HPhoton.Utils.tests
        ]
        
main = defaultMain tests
