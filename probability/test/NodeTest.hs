module NodeTest where

import Node
import Test.HUnit

-- Testing localProbability Function
testAtomic1 = TestCase (assertEqual "probabilities match" probs realProbs) where
  -- Simple test for an Atomic, Unobserved node
  node1 = createNode 0 [] [] (Unobserved $ Bernoulli 0.4)
  prob1 = localProbability node1 [True]
  prob2 = localProbability node1 [False]
  probs = (prob1, prob2)
  realProbs = (0.4, 0.6)

testCompound1 = TestCase (assertEqual "probabilities match" probs realProbs) where
  -- Testing function with a network with 2 variables
  node1 = createNode 0 [] [1] (Unobserved $ Bernoulli 0.3)
  node2 = createNode 1 [0] [] (Unobserved $ CPD1 0.2 0.4)

  -- Testing Atomic
  prob1 = localProbability node1 [True]
  prob2 = localProbability node1 [False]

  -- TEsting Compound
  prob3 = localProbability node2 [True, True]
  prob4 = localProbability node2 [True, False]
  prob5 = localProbability node2 [False, True]
  prob6 = localProbability node2 [False, False]

  probs = (prob1, prob2, prob3, prob4, prob5, prob6)
  realProbs = (0.3, 0.7,  0.2, 0.8, 0.4, 0.6)


tests = TestList [TestLabel "Atomic Test #1" testAtomic1,
                  TestLabel "Compound Test #1" testCompound1]


-- shortcut to run tests
runTests = runTestTT tests
