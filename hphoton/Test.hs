module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified HPhoton.BurstIdent.Bayes.Tests
import qualified HPhoton.Bin.Tests
import qualified HPhoton.Utils.Tests
  
tests = [ testGroup "BurstIdent.Bayes" HPhoton.BurstIdent.Bayes.Tests.tests
        , testGroup "Bin" HPhoton.Bin.Tests.tests
        , testGroup "Utils" HPhoton.Utils.Tests.tests
        ]
        
main = defaultMain tests
