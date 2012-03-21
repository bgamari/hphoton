module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified HPhoton.BurstIdent.Bayes
  
tests = [ testGroup "BurstIdent.Bayes" HPhoton.BurstIdent.Bayes.tests
        ]
        
main = defaultMain tests
