module UniverseTest where

import Universe
import Test.HUnit
import Node
import qualified Data.Map.Strict as Map
-- Construct a universe without the 'add' method for testing purposes-- 

s = initialState 0

-- create nodes
a = createNode 0 [] [1] (Unobserved $ Bernoulli 0.2)
b = createNode 1 [0] [2] (Unobserved $ CPD1 0.3 0.4)
c = createNode 2 [1] [] (Unobserved $ CPD1 0.65 0.84)

-- populate state
s' = s {
        next=3,
        universe=Map.fromList [(0, a), (1, b), (2, c)],
        names=Map.fromList [("a", 0), ("b", 1), ("c", 2)],
        ids=Map.fromList [(0, "a"), (1, "b"), (2, "c"), (3, "d")]
       }

-- Test for notifySelect method
testNotifySelect = TestCase (assertEqual "parents match" foundParents realParents) where
  -- alter parents of some nodes
  universe' = notifySelect (universe s') (\x -> x {parents=[2, 4, 6, 8]}) [0, 1]

  -- retrieve parents of each node
  aps = fmap parents $ Map.lookup 0 universe'
  bps = fmap parents $ Map.lookup 1 universe'
  cps = fmap parents $ Map.lookup 2 universe'

  -- values to compare
  foundParents = (aps, bps, cps)
  realParents = (Just [2, 4, 6, 8], Just [2, 4, 6, 8], Just [1])



-- List of Tests
tests = TestList [TestLabel "Test notifySelect" testNotifySelect]

-- shortcut to run tests
runTests = runTestTT tests



