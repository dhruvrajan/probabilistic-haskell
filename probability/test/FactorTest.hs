module FactorTest where

import Test.HUnit
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Maybe
import Factor


testDeleteAt = TestList [
  "empty" ~:  emptyExpected ~=? emptyReal,
  "single" ~: singleExpected ~=? singleReal,
  "last" ~: lastExpected ~=? lastReal]
  --"invalid" ~: invalidExpected ~=? invalidReal]
  where
    -- delete on an empty vector
    emptyExpected = V.fromList []
    emptyReal = deleteAt 0 $ V.fromList [1]
    -- delete with a singleton
    singleExpected = V.fromList []
    singleReal = deleteAt 0 $ V.fromList [1]
    -- delete the last element in a vector
    lastExpected = V.fromList [1, 2, 3]
    lastReal = deleteAt 3 $ V.fromList [1, 2, 3, 4]
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


tests = TestList [testDeleteAt, testMarginalize]

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
     
