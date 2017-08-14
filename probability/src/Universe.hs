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


-- | Build an initial state from a starting Node id
-- Initializes maps to the empty map
initialState :: Int -> State
initialState i = State {next=i, universe=Map.empty, names=Map.empty, ids=Map.empty}

-- | Apply a function to every node in a universe
notifyAll :: (Node -> Node) -> Universe -> Universe
notifyAll = Map.map

-- | Apply a function to a subset of a universe
notifySelect :: Universe -> (Node -> Node) -> [Int] -> Maybe Universe
notifySelect universe _ [] = Just universe
notifySelect universe f (n:ns) = do
  -- lookup the next node
  node <- Map.lookup n universe

  -- calculate new universe by applying f
  let universe' = Map.insert n (f node) universe

  -- recursively notify the rest of the nodes
  result <- notifySelect universe' f ns
 
  return result

-- | Apply a function to the parents of a particular node
notifyParents :: Universe -> Int -> Maybe Universe
notifyParents universe n = do
  -- retrieve target node, parents
  node <- Map.lookup n universe
  let select = parents node

  -- notify function: make target node a child of its parents
  let notify = \ node -> node {children=n : (children node)}

  -- notify parents
  notified <- notifySelect universe notify select
  return notified

-- | Apply a function to the children of a particular node
-- If the node is not in the universe, returns Nothing
notifyChildren :: Universe -> Int -> Maybe Universe
notifyChildren universe n = do
  -- retrieve target node, children
  node <- Map.lookup n universe
  let select = children node

  -- notify function: make target node a parent of its children
  let notify = \ node -> node {parents=n : (parents node)}

  -- notify children
  notified <- notifySelect universe notify select
  return notified

-- | Notify the universe of an incoming node
-- i.e. notify parents & children of target node
notifyUniverse :: Universe -> Node -> Maybe Universe
notifyUniverse universe (Node {identity=incoming}) = do
  parentsNotified <- notifyParents universe incoming
  childrenNotified <- notifyChildren parentsNotified incoming
  return childrenNotified

-- | Submit a new node to a universe, and notify the universe appropriately
submit :: State -> String -> Node -> (Node, State)
submit state name node = result where
  n = next state
  node' = node {identity=n}
  universe' = Map.insert n node' (universe state)
  
  newNames = Map.insert name n $ names state
  universe'' = notifyUniverse universe' node'
  newIds = Map.insert n name $ ids state

  state' = fmap (\u -> state {next=n + 1, universe=u, names=newNames, ids=newIds}) universe''
  result = fromJust $  fmap (\s -> (node', s)) state'

addBernoulli :: State -> String -> Double -> (Node, State)
addBernoulli state name p = submit state name (createDetachedNode $ Unobserved $ Bernoulli p)

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
  = submit state name (createNode 0 [getNodeId state parent] [] $ Unobserved $ CPD1 ift iff)
