module HPhoton.Utils.Tests (tests) where

import HPhoton.Types
import HPhoton.Utils

import qualified Data.Vector.Unboxed as V

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (State, Test)

testSpans :: [Span] -> [Test]
testSpans spans = map (uncurry testCase)
  [ ( "Number of spans"
    , assertBool "Wrong number of spans"
      $ length res == length spans
    )
  , ( "Number of timestamps"
    , assertBool "Wrong number of photons in span"
      $ all id
      $ zipWith (\(start,end) times->end-start == fromIntegral (V.length times)) spans res
    )
  , ( "Timestamp ranges"
    , assertBool "Excluded photon in span"
      $ all id
      $ zipWith (\(start,end) times->V.all (\x->x >= start && x < end) times) spans res
    )
  ]
  where times = V.enumFromN 0 1000
        res = spansPhotons times spans
        
tests = [ testGroup "Zero length span" $ testSpans [(0,0)]
        , testGroup "One length span" $ testSpans [(0,1)]
        ]