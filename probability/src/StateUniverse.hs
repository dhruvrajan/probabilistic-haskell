{-# LANGUAGE ExistentialQuantification #-}
module StateUniverse where

import Control.Monad.State.Strict
import Distribution
import Data.List (find)
import qualified Data.Map as Map

data Observation a = Observed a | Unobserved

-- a factor representing a conditional probability distribution
type Factor = [[Double]]

-- represent the type of a variable. Either a distribution or a CPD
data Payload a =
  -- an atomic variable, with a discrete distribution
  forall d. (Discrete d) => DiscreteAtomic {discreteDist :: (d a)}
  -- a continuous variable, with a continuous distribution
  | forall d. (Continuous d) => ContinousAtomic {contDist :: (d a)}
  -- a compound variable, defind by CPDs over existing variables
  | Compound [Factor]

data Variable = forall a. Variable {
  name :: String,
  identity :: Int,
  payload :: Payload a,
  observation :: Observation a,
  parents :: [Int],
  children :: [Int]}

instance Show Variable where
  show var = show (name var, identity var)

-- representing a connection between two variables
type Dependency = (String, String)

-- a network of random variables
type Network = (Int, Map.Map Int Variable, [Dependency])

-- retrieve a variable object by its name, if it exists
retrieveByName :: String -> State Network (Maybe Variable)
retrieveByName str = do (i, bn, edges) <- get
                        return $ find (\x -> str == name x) bn

-- retrieve a variable object by its id, if it exists
retrieveById :: Int -> State Network (Maybe Variable)
retrieveById id = do (i, bn, edges) <- get
                     return $ find (\x -> id == identity x) bn
                          
-- an empty network, ids start at 0
empty :: Network
empty = (0, Map.empty, [])

-- submit a variable to a network
-- TODO: check whether variable is already in the network
variable :: String -> Payload a ->  State Network (Maybe Variable)
variable name dist = do (i, bn, edges) <- get
                        -- create the corresponding variable
                        let var = Variable name i dist Unobserved [] []
                        let bn' = Map.insert i var bn
                        put (i+1,bn', edges)
                        return $ Just var

-- submit a dependency to a network
dependency :: String -> String -> State Network (Maybe Dependency)
dependency var1 var2 = do (i, bn, edges) <- get
                          -- create a dependency
                          let dep = (var1, var2)
                          -- replace new network
                          put (i, bn, (dep:edges))
                          return $ Just dep

-- A bernoulli variable
flip :: String -> Double -> State Network (Maybe Variable)
flip name prob = variable name $ DiscreteAtomic $ Bernoulli prob True False

example :: State Network (Maybe Variable)
example = do coin1 <- StateUniverse.flip "coin1" 0.3
             coin2 <- StateUniverse.flip "coin2" 0.4
             return coin1
