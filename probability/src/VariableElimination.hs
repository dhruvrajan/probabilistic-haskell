module VariableElimination (getFactor) where 

import Node
import Universe
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.List
import Control.Monad (join)
import Debug.Trace
import Factor

-- | Generate the local factor for a specific node
getFactor :: State -> Int -> Maybe Factor
getFactor state nodeId = do
  --nodeId < getId state name
  name <- getName state nodeId
  node <- getNode state name

  let factorVars = reverse $ sort $ nodeId : (parents $ node)
  let keys = boolPermutations $ length factorVars
  let probs = map (localProbability node) keys
  let joint = Factor factorVars (M.fromList (zip keys probs))

  -- if node is observed, then filter the full joint distribution
  case node of
    Node {payload=Unobserved _} -> Just joint
    Node {payload=Observed _ val, identity=i} ->
      if (length $ Factor.ids joint) == 1 then filterZero joint i val else filterFactor joint i val


test = do
  let s0 = empty
  (a, s1) <- addBernoulli s0 "a" 0.4 $ Just True
  (b, s2) <- addCPD1 s1 "b" "a" 0.2 0.3 $ Nothing

  fa <- getFactor s2 0
  fb <- getFactor s2 1

  -- -- probability that a is true
  prod <- (pointwiseProduct fa fb)

  return (fa, fb, s2)

-- eliminate :: State -> [Int] -> Int -> Maybe Factor
-- eliminate _ [] _ = Just nullFactor
-- eliminate state (v:vs) t = do
--   name <- getName state v
--   node <- getNode state name
--   let factor = getFactor state v
--   adjusted <- if observed node
--               then factor
--               else
--                 if v == t
--                 then factor
--                 else factor >>= marginalize v
  
  -- adjusted <- case node of
  --             Node {payload=Observed _ val} -> factor
  --             Node {payload=Unobserved _} -> if v == t
  --                                            then factor
  --                                            else factor >>= marginalize v
  -- result <- if vs == [] then Just adjusted
  --           else eliminate state vs t >>= pointwiseProduct adjusted
  --return result

-- eliminate :: State -> [Int] -> Int -> [Maybe Factor]
-- eliminate _  [] _ = []
-- eliminate state (v:vs) t = do
--   name <- getName state v
--   node <- getNode state name

--   let factor = getFactor state v

--   let adjusted = if observed node
--               then factor
--               else
--                 if v == t
--                 then factor
--                 else factor >>= marginalize v

  -- result <- if vs == [] then Just adjusted
  --           else eliminate state vs (adjusted:fs) t >>= pointwiseProduct adjusted

prod :: [Maybe Factor] -> Maybe Factor
prod [] = Just nullFactor
prod (f:fs) = do
  factor <- f
  remaining <- prod fs

  if fs == [] then f else pointwiseProduct factor $ remaining



eliminate :: State -> [Int] -> [Maybe Factor] -> Int -> Maybe [Maybe Factor]
eliminate _ [] fs _ = Just fs
eliminate state (v:vs) fs t = do
  name <- getName state v
  node <- getNode state name
  
  let factor = getFactor state v
  let fs' = (factor:fs)
  let fs'' = if (observed node)
             then let result =  (map (\f -> f >>= (\ f' -> filterFactor f' v (fromJust $ observation node))) (factor:fs))
                  in traceShow result result
             else if v == t
                  then fs'
                  else map (\f -> f >>= marginalize v) fs'
  -- result <- traceShow (v, fs'') $ if vs == [] then prod fs''
  --           else eliminate state vs fs'' t
  result <- if vs == [] then Just fs'' else eliminate state vs fs'' t
  return $ result
  
  


-- | Variable Elimination Algorithm
-- Returns a distribution over the target variable
elimination :: State -> String -> Maybe Factor
elimination state target = do
  let order = reverse $ take (next state) [0..]
  targetId <- getId state target

  normalize <$> (prod $ fromJust $ eliminate state order [] targetId)
  
  


-- sumOutIfNecessary target (node, factor) = case node of
--                                      Node {payload= Unobserved _, identity=i} -> if i == (identity target)
--                                                                                  then Just factor
--                                                                                  else marginalize i factor
--                                      Node {payload=Observed _ val} -> Just factor

-- accumProduct factors = foldl (\x y -> join $ pointwiseProduct <$> x <*> y) (head factors) (tail factors)

-- elimination :: State -> String -> Maybe Factor
-- elimination state varName = do
--   let targetId = getId state varName
--   let targetNode = getNode state varName
--   let order = take (next state) [0..]

--   let names = map (getName state) order
--   let nodes = map (\x -> join $ getNode state <$> x) names

--   let factors =  map (\x -> x >>= getFactor state) names
--   let pairs = map (\(Just x, Just y) -> sumOutIfNecessary (fromJust targetNode) (x, y)) $ zip nodes factors

--     -- calculate factor product
--   let product = accumProduct factors

--   normalize <$> product 
  


