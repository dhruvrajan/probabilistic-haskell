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
-- | Initializes maps to the empty map
initialState :: Int -> State
initialState i = State {next=i, universe=Map.empty, names=Map.empty, ids=Map.empty}

notifyAll :: (Node -> Node) -> Universe -> Universe
notifyAll = Map.map


-- | Apply a function to a selection of a universe.
-- Ignores node ids in the selection not present in the universe
notifySelect :: Universe -> (Node -> Node) -> [Int] -> Universe
notifySelect universe _ [] = universe
notifySelect universe f (n:ns) = notifySelect universe' f ns where
  nodeLookup = Map.lookup n universe
  universe' = case nodeLookup of
                Just node -> Map.insert n node' universe where
                  node' = f node
                Nothing -> universe

-- | Apply a function to the parents of a particular node
-- If the node is not in the universe, returns Nothing
notifyParents :: Universe -> Int -> Maybe Universe
notifyParents universe n = fmap (notifySelect universe f) select
  where
    select = fmap parents $ Map.lookup n universe
    f = \ node -> node {children=n : (children node)}

-- | Apply a function to the children of a particular node
-- If the node is not in the universe, returns Nothing
notifyChildren :: Universe -> Int -> Maybe Universe
notifyChildren universe n = fmap (notifySelect universe f) select where
  select = fmap children $ Map.lookup n universe
  f = \ node -> node {parents=n : (parents node)}

-- | Notify the universe of an incoming node
-- | If node is not in universe, returns Nothing
notifyUniverse :: Universe -> Node -> Maybe Universe
notifyUniverse universe (Node {identity=incoming, parents=ps, children=cs})
  = universe''
  where
    universe' = notifyParents universe incoming
    universe'' = universe' >>= (\u -> notifyChildren u incoming) 

add :: State -> String -> Node -> (Node, State)
add state name node = result where
  n = next state
  node' = node {identity=n}
  universe' = Map.insert n node' (universe state)
  newNames = Map.insert name n $ names state
  universe'' = notifyUniverse universe' node'
  newIds = Map.insert n name $ ids state

  state' = fmap (\u -> state {next=n + 1, universe=u, names=newNames, ids=newIds}) universe''
  result = fromJust $  fmap (\s -> (node', s)) state'

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

-- s = initialState 0
-- (b, s') = addBernoulli s "b" 0.4
-- (c, s'') = addBernoulli s' "c" 0.5
-- (d, s''') = addBernoulli s'' "d" 0.6
-- (e, s'''') = addCPD s''' "e" "b" 0.5 0.2
