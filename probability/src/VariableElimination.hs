module VariableElimination where

import Node
import Universe
import Numeric.LinearAlgebra.HMatrix as HM
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.List

data Factor = Factor [Int] (Map.Map [Bool] Double) deriving (Show)

boolPermutations :: Int -> [[Bool]]
boolPermutations 0 = [[]]
boolPermutations n = let ps = boolPermutations (n - 1)
                      in union  [True:cs | cs <- ps] [False:cs | cs <- ps]

getFactor :: State -> String -> Factor
getFactor state name = factor where
  nodeId = getNodeId state name
  factorVars = reverse $ sort $ nodeId : (parents $ getNode state name)
  keys = boolPermutations $ length factorVars
  node = getNode state name
  probs = map (localProbability node) keys
  factor = Factor factorVars (Map.fromList (zip keys probs))


projectFactor :: Factor -> Bool -> Factor
projectFactor (Factor keys table)  bool
  = Factor keys (Map.filterWithKey (\k _ -> last k == bool) table)

multiplyFactors :: Factor -> Factor -> Factor



-- data CPD = CPD { var :: String, pars :: [String],  table :: Map.Map ([Bool], Bool) Double } deriving(Show)
-- -- CPD (factor name) (parent names) (Matrix)

-- getCPD :: State -> Node -> CPD
-- getCPD state Node{payload=Unobserved (Bernoulli p), identity=n, parents=ps}
--   = CPD (getName state n) (map (getName state) ps)
--   $ Map.fromList [(([], True), p), (([], False), 1-p)]

-- getCPD state Node{payload=Unobserved (CPD1 ift iff), identity=n, parents=ps}
--   = CPD (getName state n)  (map (getName state) ps)
--   $ Map.fromList [(([True], True), ift), (([True], False), 1-ift),
--                   (([False], True), iff), (([False], False), 1-iff)]

-- getFactor :: State -> Node -> CPD
-- getFactor state node = case node of
--                          Node{payload=Observed _ obs} -> let cpd = getCPD state node
--                                                          in CPD (var cpd) (pars cpd) $
--                                                             Map.fromList $ filter (\ ((_, val), _) -> val == obs) $
--                                                             Map.toList $ table $ cpd
                                                           

--                          Node{payload=Unobserved _} -> getCPD state node

-- pointwiseProduct :: CPD -> CPD -> CPD
-- pointwiseProduct CPD{var=var1, pars=pars1, table=table1} CPD{var=var2, pars=pars2, table=table2}
--   | pars1 == pars2 = CPD var1 (pars1) $ Map.fromList $ map (\(k, v)->
-- (k, v *  (fromJust $ Map.lookup k table2))) $ Map.toList table1

-- elimination :: State -> String -> Int -> CPD -> CPD
-- elimination State{next=n1} _ n2 last | n1 == n2 = last
-- elimination state name n last | pars last = pointwiseProduct last (getCPD state name)


-- getCPD state Node{payload=Observed _ value, identity=n, parents=ps}
--   = CPD (getName state n) (map (getName state ) ps)
--   $ Map.fromList [(([], True), if value then 1.0 else 0.0),
--                   (([], False), if not value then 1.0 else 0.0)]

s = initialState 0
(a, s') = addBernoulli s "a" 0.3
(b, s'') = addCPD s' "b" "a" 0.4 0.5
--TODO: Check Dimensions of matrices somehow
  
