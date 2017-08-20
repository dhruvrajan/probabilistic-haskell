module VariableEliminationTest where

import Node
import Universe
import Factor
import VariableElimination

import Test.HUnit
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map

s = empty

-- create nodes
a = createNode 0 [] [1] (Unobserved $ Bernoulli 0.2)
b = createNode 1 [0] [2] (Unobserved $ CPD1 0.3 0.4)
c = createNode 2 [1] [] (Unobserved $ CPD1 0.65 0.84)

-- populate state
s' = s {
        next=3,
        universe=Map.fromList [(0, a), (1, b), (2, c)],
        names=Map.fromList [("a", 0), ("b", 1), ("c", 2)],
        Universe.ids=Map.fromList [(0, "a"), (1, "b"), (2, "c"), (3, "d")]
       }


tf1 = Factor [1, 0] $ Map.fromList [([True, True], 0.3),
                                     ([True, False], 0.9),
                                     ([False, True], 0.7),
                                     ([False, False], 0.1)]
       
tf2 = Factor [2, 1] $ Map.fromList [([True, True], 0.2),
                                     ([True, False], 0.6),
                                     ([False, True], 0.8),
                                     ([False, False], 0.4)]

-- Burglary Network from Russel Norvig Book


-- Testing boolean permutations method
testBoolPermutations = TestCase (assertEqual "boolean permutations" real vals) where
  perms = boolPermutations 5
  vals = (length perms, perms)
  real = (2 ^ 5, nub perms)

-- Testing getFactor
testGetFactor = TestCase (assertEqual "factors from graph" real vals) where
  fa = getFactor s' "a"
  fb = getFactor s' "b"
  fc = getFactor s' "c"

  vals = [fa, fb, fc]
  real = map Just  [Factor [0] $ Map.fromList [([True], 0.2), ([False], 0.8)],
          Factor [1, 0] $ Map.fromList [([True, True], 0.3),
                                        ([True, False], 0.7),
                                        ([False, True], 0.4),
                                        ([False, False], 0.6)],
           Factor [2, 1] $ Map.fromList [([True, True], 0.65),
                                         ([True, False], 1 - 0.65),
                                         ([False, True], 0.84),
                                         ([False, False], 1-0.84)]]

testPointwiseProduct = TestCase (assertEqual "pointwise product of factors" real vals) where

  --- from example above
  fa = fromJust $ getFactor s' "a"
  fb = fromJust $ getFactor s' "b"
  fc = fromJust $ getFactor s' "c"

  p1 = pointwiseProduct fa fb
  r1 = Factor [1, 0] $ Map.fromList [([True, True], 0.3 * 0.2),
                                     ([True, False], 0.7 * 0.8),
                                     ([False, True], 0.4 * 0.2),
                                     ([False, False], 0.6 * 0.8)]

  p2 = pointwiseProduct fb fc
  r2 = Factor [2, 1, 0] $ Map.fromList[([True, True, True], 0.3 * 0.65),
                                      ([True, True, False], 0.7 * 0.65),
                                      ([True, False, True], 0.4 * (1-0.65)),
                                      ([True, False, False], 0.6 * (1-0.65)),
                                      ([False, True, True], 0.3 * 0.84),
                                      ([False, True, False], 0.7 * 0.84),
                                      ([False, False, True], 0.4 * (1-0.84)),
                                      ([False, False, False], 0.6 * (1-0.84))]

  --- from Norvig book page 527
  f1 = Factor [1, 0] $ Map.fromList [([True, True], 0.3),
                                     ([True, False], 0.9),
                                     ([False, True], 0.7),
                                     ([False, False], 0.1)]
       
  f2 = Factor [2, 1] $ Map.fromList [([True, True], 0.2),
                                     ([True, False], 0.6),
                                     ([False, True], 0.8),
                                     ([False, False], 0.4)]

  p3 = pointwiseProduct f1 f2

  r3 = Factor [2, 1, 0] $ Map.fromList [([True, True, True], 0.06),
                                        ([True, True, False], 0.18),
                                        ([True, False, True], 0.42),
                                        ([True, False, False], 0.06),
                                        ([False, True, True], 0.24),
                                        ([False, True, False], 0.72),
                                        ([False, False, True], 0.28),
                                        ([False, False, False], 0.04)]
       
  vals = (p1, p2)
  real = (r1, r2)

testSumOut = TestCase (assertEqual "sum out a variable" real vals) where
  --- from Norvig book page 527
  f1 = Factor [1, 0] $ Map.fromList [([True, True], 0.3),
                                     ([True, False], 0.9),
                                     ([False, True], 0.7),
                                     ([False, False], 0.1)]
       
  f2 = Factor [2, 1] $ Map.fromList [([True, True], 0.2),
                                     ([True, False], 0.6),
                                     ([False, True], 0.8),
                                     ([False, False], 0.4)]

  
  p3 = pointwiseProduct f1 f2 -- this is verified to be correct in testPointwiseProduct

  s1 = sumOut 0 p3
  r1 = normalize $ Factor [2, 1] $ Map.fromList [([True, True], 0.24),
                                     ([True, False], 0.48),
                                     ([False, True], 0.96),
                                     ([False, False], 0.32)]

  vals=(s1)
  real=(r1)
  
tests = TestList [TestLabel "Testing Bool Permutations" testBoolPermutations,
                  TestLabel "Testing Factorize Method" testGetFactor,
                  TestLabel "Testing Pointwise Product" testPointwiseProduct,
                  TestLabel "Testing Sum Out" testSumOut]


-- shortcut to run tests
runTests = runTestTT tests
