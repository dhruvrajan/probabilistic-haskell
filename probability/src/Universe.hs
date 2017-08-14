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

-- | Build an empty state
empty :: State
empty = State {next=0, universe=Map.empty, names=Map.empty, ids=Map.empty}

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
  notifySelect universe' f ns

-- | Apply a function to the parents of a particular node
notifyParents :: Universe -> Int -> Maybe Universe
notifyParents universe n = do
  -- retrieve target node, parents
  node <- Map.lookup n universe
  let select = parents node

  -- notify function: make target node a child of its parents
  let notify = \ node -> node {children=n : (children node)}

  -- notify parents
  notifySelect universe notify select

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
  notifySelect universe notify select

-- | Notify the universe of an incoming node
-- i.e. notify parents & children of target node
notifyUniverse :: Universe -> Node -> Maybe Universe
notifyUniverse universe (Node {identity=incoming}) = do
  parentsNotified <- notifyParents universe incoming
  childrenNotified <- notifyChildren parentsNotified incoming
  return childrenNotified

-- | Submit a new node to a universe, and notify the universe appropriately
-- Returns Nothing if the target node's parents/children are not already in
-- the universe.
submit :: State -> String -> Node -> Maybe (Node, State)
submit state name node = do
  let n = next state -- next node id
  let node' = node {identity=n} -- new node
  -- insert new node into universe
  let universe' = Map.insert n node' (universe state) 

  let names' = Map.insert name n (names state) -- updated names lookup table
  let ids' = Map.insert n name (ids state) -- updated ids lookup table

  notified <- notifyUniverse universe' node' -- notify universe of incoming node
  let state' = state {next=n + 1, universe=notified, names=names', ids=ids'} -- construct new state
  return (node', state') 

getNodeId :: State -> String -> Maybe Int
getNodeId state name = Map.lookup name (names state)

getNode :: State -> String -> Maybe Node
getNode state name = do
  nodeId <- getNodeId state name
  Map.lookup nodeId (universe state)

getName :: State -> Int -> Maybe String
getName state n = Map.lookup n (ids state)

-- | Add a bernoulli element to a universe
addBernoulli :: State -> String -> Double -> Maybe (Node, State)
addBernoulli state name p = submit state name (createDetachedNode $ Unobserved $ Bernoulli p)

-- | Add a CPD1 element to a universe
addCPD1 :: State -> String -> String -> Double -> Double -> Maybe (Node, State)
addCPD1 state name parent ift iff = do --submit state name node where
  -- construct CPD1 node
  parentId <- getNodeId state parent
  let node = createNode 0 [parentId] [] $ Unobserved $ CPD1 ift iff

  -- add node to state
  submit state name node
