module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified HPhoton.BurstIdent.Bayes.Tests
import qualified HPhoton.Bin.Tests
import qualified HPhoton.Utils.Tests
import qualified HPhoton.Corr.PackedVec.Tests
import qualified HPhoton.Corr.SparseCorr.Tests
  
tests = [ testGroup "BurstIdent.Bayes" HPhoton.BurstIdent.Bayes.Tests.tests
        , testGroup "Bin" HPhoton.Bin.Tests.tests
        , testGroup "Utils" HPhoton.Utils.Tests.tests
        , testGroup "Packed Vector" HPhoton.Corr.PackedVec.Tests.tests
        , testGroup "Sparse correlator" HPhoton.Corr.SparseCorr.Tests.tests
        ]
        
main = defaultMain tests
