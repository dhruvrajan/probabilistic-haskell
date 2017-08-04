module VariableElimination where

import Node
import Universe
import Numeric.LinearAlgebra.HMatrix as HM
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.List
import Debug.Trace

-- | Factor Data Structure, representing a discrete, finite CPD
data Factor = Factor [Int] (Map.Map [Bool] Double) deriving (Show, Eq)

-- Accessor methods for Factor

keys :: Factor -> [Int]
keys (Factor l m) = l

table :: Factor -> (Map.Map [Bool] Double)
table (Factor l m) = m

-- | Generate all possible boolean vectors of a given size
boolPermutations :: Int -> [[Bool]]
boolPermutations 0 = [[]]
boolPermutations n = let ps = boolPermutations (n - 1)
                      in union  [True:cs | cs <- ps] [False:cs | cs <- ps]

-- | Generate the local factor for a specific node
getFactor :: State -> String -> Factor
getFactor state name = factor where
  nodeId = getNodeId state name
  factorVars = reverse $ sort $ nodeId : (parents $ getNode state name)
  keys = boolPermutations $ length factorVars
  node = getNode state name
  probs = map (localProbability node) keys
  factor = Factor factorVars (Map.fromList (zip keys probs))

sumTable :: (Map.Map [Bool] Double) -> Double
sumTable map = sum $ Map.elems map

normalizeFactor :: Factor -> Factor
normalizeFactor (Factor vars table) = Factor vars table' where
  sum = sumTable table
  table' = Map.map (\k -> k / sum) table

deleteAt :: Int -> [a] -> [a]
deleteAt n l = (init l1) ++ l2 where
  (l1, l2) = splitAt (n + 1) l

projectFactor :: Factor -> Int -> Bool -> Factor
projectFactor (Factor keys table) target bool = Factor keys newTable where
  targetIndex = fromJust $ elemIndex target keys
  newTable = Map.filterWithKey (\k _ -> k !! targetIndex == bool) table
  

--projectFactor (Factor keys table)  bool
--  = Factor keys (Map.filterWithKey (\k _ -> last k == bool) table)


splitPerm :: [Int] -> [Int] -> [Bool] -> ([Bool], [Bool])
splitPerm f1 f2 ps = (f1Perm, f2Perm) where
  newKeys = reverse $ sort $ union f1 f2
  enumPs = zip newKeys ps
  f1PermPairs = filter (\(nid, p) -> elem nid f1) enumPs
  f1Perm = map (\(x, y) -> y) f1PermPairs

  f2PermPairs = filter (\(nid, p) -> elem nid f2) enumPs
  f2Perm = map(\(x, y) -> y) f2PermPairs
  

pointwiseProduct :: Factor -> Factor -> Factor
pointwiseProduct f1 f2 = f3 where
  table1 = table f1
  table2 = table f2
  newKeys = reverse $ sort $ union (keys f1) (keys f2)
  perms =  boolPermutations $ length newKeys
  pairs = map ( \x -> (x, splitPerm (keys f1) (keys f2) x)) perms
  evalProbs = map (\(x, (f1_v, f2_v)) -> (x, (fromJust $ Map.lookup f1_v table1) *
                                         (fromJust $ Map.lookup f2_v table2))) pairs

  table3 = Map.fromList evalProbs
  f3 = Factor newKeys table3
  
  

f1 = Factor [0] $ Map.fromList [([True], 0.2), ([False], 0.8)]
f2 = Factor [1, 0] $ Map.fromList [([True, True], 0.3), ([True, False], 0.4),
                                   ([False, True], 0.7), ([False, False], 0.6)]

table1 = table f1
table2 = table f2
newKeys = reverse $ sort $ union (keys f1) (keys f2)
perms = boolPermutations $ length newKeys
pairs = map ( \x -> (x, splitPerm (keys f1) (keys f2) x)) perms
evalProbs = map (\(x, (f1_v, f2_v)) -> (x, (fromJust $ Map.lookup f1_v table1) *
                                         (fromJust $ Map.lookup f2_v table2))) pairs

table3 = Map.fromList evalProbs
f3 = Factor newKeys table3



  
sumOut :: Int -> Factor -> Factor
sumOut nodeId (Factor vars table)  = normalizeFactor $ Factor newVars newValues where
  targetIdx = fromJust $ elemIndex nodeId vars
  -- Target is True
  isTrue = Map.filterWithKey (\k _ -> k !! targetIdx == True) table
  isTrue' = Map.mapKeys (\k1 -> deleteAt targetIdx k1) isTrue

  -- Target is False
  isFalse = Map.filterWithKey (\k _ -> k !! targetIdx == False) table
  isFalse' = Map.mapKeys (\k1 -> deleteAt targetIdx k1) isFalse

  -- TODO: Proper Maybe Handling
  newValues = Map.mapWithKey (\k v -> v + (fromJust $ Map.lookup k isFalse')) isTrue'

  --newKeys = Map.mapKeys (\k1 -> deleteAt targetIdx k1) newValues
  newVars = delete nodeId vars


-- s = initialState 0
-- (a, s') = addBernoulli s "a" 0.3
-- (b, s'') = addCPD s' "b" "a" 0.4 0.5


-- --factor = getFactor s'' "b"
-- factor= Factor [2, 1, 0] $ Map.fromList [([True, True, True], 0.06),
--                                         ([True, True, False], 0.18),
--                                         ([True, False, True], 0.42),
--                                         ([True, False, False], 0.06),
--                                         ([False, True, True], 0.24),
--                                         ([False, True, False], 0.72),
--                                         ([False, False, True], 0.28),
--                                         ([False, False, False], 0.04)]
-- nodeId = 0

-- vars = keys factor
-- t = table factor

-- targetIdx = fromJust $ elemIndex nodeId vars
--  -- Target is True
-- isTrue = Map.filterWithKey (\k _ -> k !! targetIdx == True) t
-- isTrue' = Map.mapKeys (\k1 -> deleteAt targetIdx k1) isTrue

-- -- Target is False
-- isFalse = Map.filterWithKey (\k _ -> k !! targetIdx == False) t
-- isFalse' = Map.mapKeys (\k1 -> deleteAt targetIdx k1) isFalse

-- -- TODO: Proper Maybe Handling
-- newValues = Map.mapWithKey (\k v -> v + (fromJust $ Map.lookup k isFalse')) isTrue'

-- newVars = delete nodeId vars

-- normalizedValues = normalizeFactor $ Factor newVars newValues

  
