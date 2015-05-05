module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified HPhoton.Corr.PackedVec.Tests
import qualified HPhoton.Corr.SparseCorr.Tests

tests = testGroup "Tests"
    [ HPhoton.Corr.PackedVec.Tests.tests
    , HPhoton.Corr.SparseCorr.Tests.tests
    ]

main = defaultMain tests
