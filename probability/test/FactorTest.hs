module FactorTest where

import Test.HUnit
--import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Factor
import Universe
import Node
--import VariableElimination (getFactor)



-- | Generate the local factor for a specific node
getFactor :: Universe.State -> String -> Factor
getFactor state name = factor where
  nodeId = getNodeId state name
  --node = getNode state name

  -- TODO: FIX BUG 
  -- parentNodes = parents node
  -- parentKeys = boolPermutations $ length parentNodes
  -- nodeKeys = [True, False]
  
  -- ks = [(pkeys, nkey) | pkeys <- parentKeys, nkey <- nodeKeys]
  -- probs = map (\(ps, n) -> localProbability node ps n) ks

  -- factorKeys  = map (\(ps, n) -> reverse $ sort $ ps ++ [n]) ks
  -- factorVars = reverse $ sort $ nodeId : parentNodes
  -- factor = Factor factorVars (Map.fromList (zip factorKeys probs))
  
  factorVars = reverse $ sort $ nodeId : (parents $ getNode state name)
  keys = boolPermutations $ length factorVars
  node = getNode state name
  probs = map (localProbability node) keys
  factor = Factor factorVars (M.fromList (zip keys probs))

testDeleteAt = TestList [
  "empty" ~:  emptyExpected ~=? emptyReal,
  "single" ~: singleExpected ~=? singleReal,
  "last" ~: lastExpected ~=? lastReal]
  --"invalid" ~: invalidExpected ~=? invalidReal]
  where
    -- delete on an empty vector
    emptyExpected = []
    emptyReal = deleteAt 0 $ [1]
    -- delete with a singleton
    singleExpected =  []
    singleReal = deleteAt 0 $ [1]
    -- delete the last element in a vector
    lastExpected = [1, 2, 3]
    lastReal = deleteAt 3 $ [1, 2, 3, 4]
    -- delete at an invalid index (should do nothing)
    --invalidExpected = V.fromList [1, 2, 3, 4]
    --invalidReal = deleteAt 6 $ V.fromList [1, 2, 3, 4]


testMarginalize = TestList [
  "compare x" ~: xExpected ~=? x,
  "compare y" ~: yExpected ~=? y
                           ]
  where
    -- X = Flip(0.2)
    -- Y = CPD (X, T->0.4, F->0.3)
    fullJoint = createFactor [1, 0] [
      ([True, True], 0.4 * 0.2),
      ([True, False], 0.3 * 0.8),
      ([False, True], 0.6 * 0.2),
      ([False, False], 0.7 * 0.8)]

    x = fromJust $ marginalize fullJoint 0
    y = fromJust $ marginalize fullJoint 1

    xExpected = createFactor [1] [([False],0.6799999999999999),([True],0.32)]
    yExpected = createFactor [0] [([False],0.7999999999999999),([True],0.2)]











--- =================================================================
-- createState
s = initialState 0

-- create nodes
a = createNode 0 [] [1] (Unobserved $ Bernoulli 0.2)
b = createNode 1 [0] [2] (Unobserved $ CPD1 0.3 0.4)
c = createNode 2 [1] [] (Unobserved $ CPD1 0.65 0.84)

-- populate state
s' = s {
        next=3,
        universe=M.fromList [(0, a), (1, b), (2, c)],
        names=M.fromList [("a", 0), ("b", 1), ("c", 2)],
        Universe.ids=M.fromList [(0, "a"), (1, "b"), (2, "c"), (3, "d")]
       }


tf1 = Factor [1, 0] $ M.fromList [([True, True], 0.3),
                                     ([True, False], 0.9),
                                     ([False, True], 0.7),
                                     ([False, False], 0.1)]
       
tf2 = Factor [2, 1] $ M.fromList [([True, True], 0.2),
                                     ([True, False], 0.6),
                                     ([False, True], 0.8),
                                     ([False, False], 0.4)]


testBoolPermutations = TestCase (assertEqual "boolean permutations" real vals) where
  perms = boolPermutations 5
  vals = (length perms, perms)
  real = (2 ^ 5, nub perms)


testPointwiseProduct = TestCase (assertEqual "pointwise product of factors" real vals) where

  --- from example above
  fa = getFactor s' "a"
  fb = getFactor s' "b"
  fc = getFactor s' "c"

  p1 = pointwiseProduct fa fb
  r1 = Factor [1, 0] $ M.fromList [([True, True], 0.3 * 0.2),
                                     ([True, False], 0.7 * 0.8),
                                     ([False, True], 0.4 * 0.2),
                                     ([False, False], 0.6 * 0.8)]

  p2 = pointwiseProduct fb fc
  r2 = Factor [2, 1, 0] $ M.fromList[([True, True, True], 0.3 * 0.65),
                                      ([True, True, False], 0.7 * 0.65),
                                      ([True, False, True], 0.4 * (1-0.65)),
                                      ([True, False, False], 0.6 * (1-0.65)),
                                      ([False, True, True], 0.3 * 0.84),
                                      ([False, True, False], 0.7 * 0.84),
                                      ([False, False, True], 0.4 * (1-0.84)),
                                      ([False, False, False], 0.6 * (1-0.84))]

  --- from Norvig book page 527
  f1 = Factor [1, 0] $ M.fromList [([True, True], 0.3),
                                     ([True, False], 0.9),
                                     ([False, True], 0.7),
                                     ([False, False], 0.1)]       
       
  f2 = Factor [2, 1] $ M.fromList [([True, True], 0.2),
                                     ([True, False], 0.6),
                                     ([False, True], 0.8),
                                     ([False, False], 0.4)]

  p3 = pointwiseProduct f1 f2

  r3 = Factor [2, 1, 0] $ M.fromList [([True, True, True], 0.06),
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
  f1 = Factor [1, 0] $ M.fromList [([True, True], 0.3),
                                     ([True, False], 0.9),
                                     ([False, True], 0.7),
                                     ([False, False], 0.1)]
       
  f2 = Factor [2, 1] $ M.fromList [([True, True], 0.2),
                                     ([True, False], 0.6),
                                     ([False, True], 0.8),
                                     ([False, False], 0.4)]

  
  p3 = pointwiseProduct f1 f2 -- this is verified to be correct in testPointwiseProduct

  s1 = sumOut 0 p3
  r1 = normalize $ Factor [2, 1] $ M.fromList [([True, True], 0.24),
                                     ([True, False], 0.48),
                                     ([False, True], 0.96),
                                     ([False, False], 0.32)]

  vals=(s1)
  real=(r1)
  
-- ===================================================================




tests = TestList [testDeleteAt, testMarginalize, testPointwiseProduct, testSumOut]

runTests = runTestTT tests


-- testCreateTable = TestList [
--   "test" ~: e ~=? r
--                            ]
--   where
--     t = createTable [
--       ([True, True], 0.5),
--       ([True, False], 0.4),
--       ([False, True], 0.7),
--       ([False, False], 0.4)]

--     factor = Factor (V.fromList [1, 0]) t
--     e = factor
--     r = factor

-- testCreateFactor = TestList [
--   "test" ~: e ~=? r
--                             ]
--   where
--     factor = createFactor [1, 0] [
--       ([True, True], 0.5),
--       ([True, False], 0.4),
--       ([False, True], 0.7),
--       ([False, False], 0.4)]

--     e = factor
--     r = factor
     
