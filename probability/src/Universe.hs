module Universe where

import Node
import qualified Data.Map.Strict as Map
import qualified Data.Bimap as BM
import Data.Maybe

type Universe = Map.Map Int Node

data State = State
  {
    next :: Int,
    universe :: Universe,
    names :: Map.Map String Int,
    ids :: Map.Map Int String
  } deriving (Show)


initialState :: Int -> State
initialState i = State {next=i, universe=Map.empty, names=Map.empty, ids=Map.empty}

notifyAll :: (Node -> Node) -> Universe -> Universe
notifyAll = Map.map

-- | TODO: Proper Error Handling
notifySelect :: Universe -> (Node -> Node) -> [Int] -> Universe
notifySelect universe _ [] = universe
notifySelect universe f (n:ns) = notifySelect universe' f ns where
  nodeLookup = Map.lookup n universe
  universe' = case nodeLookup of
                Just node -> Map.insert n node' universe where
                  node' = f node
                Nothing -> universe

-- | TODO: Proper Error Handling
-- | Requires that node is in the Universe to begin with
notifyParents :: Universe -> Int -> Universe
notifyParents universe n = notifySelect universe f select where
  select = fromJust $ fmap parents $ Map.lookup n universe
  f = \ node -> node {children=n : (children node)}

-- | TODO: Proper Error Handling
-- | Requires that node is in the Universe to begin with
notifyChildren :: Universe -> Int -> Universe
notifyChildren universe n = notifySelect universe f select where
  select = fromJust $ fmap children $ Map.lookup n universe
  f = \ node -> node {parents=n : (parents node)}

notifyUniverse :: Universe -> Node -> Universe
notifyUniverse universe (Node {identity=incoming, parents=ps, children=cs})
  = universe''
  where
    universe' = notifyParents universe incoming
    universe'' = notifyChildren universe' incoming

add :: State -> String -> Node -> (Node, State)
add state name node = (node', state') where
  n = next state
  node' = node {identity=n}
  universe' = Map.insert n node' (universe state)
  newNames = Map.insert name n $ names state
  universe'' = notifyUniverse universe' node'
  newIds = Map.insert n name $ ids state
  state' = state {next=n + 1, universe=universe'', names=newNames, ids=newIds}

addBernoulli :: State -> String -> Double -> (Node, State)
addBernoulli state name p = add state name (createDetachedNode $ Unobserved $ Bernoulli p)

-- TODO: Proper Error Handling
getNodeId :: State -> String -> Int
getNodeId state name = fromJust $ Map.lookup name (names state)

-- TODO: Proper Error Handling
getNode :: State -> String -> Node
getNode state name = fromJust $ Map.lookup (getNodeId state name) (universe state)

-- TODO: Proper Error Handling
getName :: State -> Int -> String
getName state n = fromJust $ Map.lookup n (ids state)

addCPD :: State -> String -> String -> Double -> Double -> (Node, State)
addCPD state name parent ift iff
  = add state name (createNode 0 [getNodeId state parent] [] $ Unobserved $ CPD1 ift iff)

-- addIf :: State -> String -> String -> String -> String -> (Node, State)
-- addIf state name chk thn els = add state name
--   (createNode 0 [fromJust $ Map.lookup chk (names state),
--                  fromJust $ Map.lookup thn (names state),
--                  fromJust $ Map.lookup els (names state)] [] (Unobserved ))

-- s = initialState 0
-- (b, s') = addBernoulli s "b" 0.4
-- (c, s'') = addBernoulli s' "c" 0.5
-- (d, s''') = addBernoulli s'' "d" 0.6
-- (e, s'''') = addCPD s''' "e" "b" 0.5 0.2
