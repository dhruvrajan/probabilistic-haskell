module Universe where

import Node
import qualified Data.Map.Strict as Map
import qualified Data.Bimap as BM
import Data.Maybe

-- a collection of nodes
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

-- | Notify a universe of an incoming node, given its id,
-- a list of parents, and a list of children
notifyIncoming :: Universe -> Int -> [Int] -> [Int] -> Maybe Universe
notifyIncoming universe n ps cs = do
  notifyParents <- notifySelect universe (\p -> p {children=n:(children p)}) ps
  notifyChildren <- notifySelect notifyParents (\c -> c {parents=n:(parents c)}) cs
  return notifyChildren

-- | Submit a new node to a universe, and notify the universe appropriately
-- Returns Nothing if the target node's parents/children are not already in
-- the universe.
submit :: State -> String -> Node -> Maybe (Node, State)
submit state name node = do
  let n = next state -- next node id

  -- notify universe of incoming node
  notified <- notifyIncoming (universe state) n (parents node) (children node)

  let newNode = node {identity=n} -- new node
  -- insert new node into notified universe
  let updatedUniverse = Map.insert n newNode notified
  
  let namesTable = Map.insert name n (names state) -- updated names lookup table
  let idsTable = Map.insert n name (ids state) -- updated ids lookup table

  -- construct new state
  let newState = state {next=n + 1, universe=updatedUniverse, names=namesTable, ids=idsTable}
  return (newNode, newState)

-- | Get the id of a node given its name
getId :: State -> String -> Maybe Int
getId state name = Map.lookup name (names state)

-- | Get the name of a node given its id
getName :: State -> Int -> Maybe String
getName state n = Map.lookup n (ids state)

-- | Retrieve a node given its name
getNode :: State -> String -> Maybe Node
getNode state name = do
  nodeId <- getId state name
  Map.lookup nodeId (universe state)

-- | Add a bernoulli element to a universe
addBernoulli :: State -> String -> Double -> Maybe Bool ->  Maybe (Node, State)
addBernoulli state name p val = submit state name (createDetachedNode $ payload)
  where
    payload = case val of
      Just v -> Observed (Bernoulli p) v
      Nothing -> Unobserved (Bernoulli p)
                                                                          

-- | Add a CPD1 element to a universe
addCPD1 :: State -> String -> String -> Double -> Double -> Maybe Bool -> Maybe (Node, State)
addCPD1 state name parent ift iff val = do --submit state name node where
  -- construct CPD1 node
  parentId <- getId state parent
  let payload = case val of
        Just v -> Observed (CPD1 ift iff) v
        Nothing -> Unobserved (CPD1 ift iff)
    
  let node = createNode 0 [parentId] [] $ payload

  -- add node to state
  submit state name node
