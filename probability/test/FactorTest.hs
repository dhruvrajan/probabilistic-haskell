module FactorTest where

import Test.HUnit
import qualified Data.Vector as V
import Factor


testDeleteAt = TestList [
  "empty" ~:  emptyExpected ~=? emptyReal,
  "single" ~: singleExpected ~=? singleReal,
  "last" ~: lastExpected ~=? lastReal,
  "invalid" ~: invalidExpected ~=? invalidReal]
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
    invalidExpected = V.fromList [1, 2, 3, 4]
    invalidReal = deleteAt 6 $ V.fromList [1, 2, 3, 4]

tests = TestList [testDeleteAt]

runTests = runTestTT tests
