module HPhoton.BurstIdent.Bayes.Tests (tests) where

import HPhoton.Types
import HPhoton.BurstIdent.Bayes
  
import qualified Data.Vector.Unboxed as V
  
import Test.QuickCheck.Modifiers
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  
prop_spans_conserve_time :: Timestamps -> Bool
prop_spans_conserve_time ts =
  let Timestamps times = ts
      spans = compressSpans (V.maximum times) times 
  in V.head spans == (V.head times, V.last times)
                              
testSpans :: Time -> [Time] -> [Span] -> Assertion
testSpans fuzz times spans = V.toList (compressSpans fuzz $ V.fromList times) @?= spans

test_one_spans = testSpans 10
                 [0,1,2,3,4,5,6,7,8,9,10
                 ,20,21,22,23,24,25,26,27,28,29]
                 [(0,29)]
                 
test_two_spans = testSpans 10
                 [0,1,2,3,4,5,6,7,8,9
                 ,20,21,22,23,24,25,26,27,28,29]
                 [(0,9), (20,29)]
      
     
tests = [ testProperty "spans conserve time" prop_spans_conserve_time
        , testCase "one spans" test_one_spans
        , testCase "two spans" test_two_spans
        ]
