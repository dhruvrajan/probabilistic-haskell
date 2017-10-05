{-# LANGUAGE ExistentialQuantification #-}
module StateUniverse where

import Control.Monad.State.Strict
import Distribution

data Observation a = Observed a | Unobserved

data Variable = forall a d . (Discrete d) => Variable
  { name :: String,
    identity :: Int,
    distribution :: d a,
    observation :: Observation a}

instance Show Variable where
  show var = show (name var, identity var)

-- Representing a connection between two variables
type Dependency = (String, String)

type Network = (Int, [Variable], [Dependency])

empty :: Network
empty = (0, [], [])

variable :: (Discrete d) => String -> d a ->  State Network (Maybe Variable)
variable name dist = do (i, bn, edges) <- get
                        let var = Variable name i dist Unobserved
                        put (i+1,(var:bn), edges)
                        return (Just var)

dependency :: String -> String -> State Network (Maybe Dependency)
dependency var1 var2 = do (i, bn, edges) <- get
                          let dep = (var1, var2)
                          put (i, bn, (dep:edges))
                          return $ Just dep
                   
example :: State Network (Maybe Variable)
example = do winter <- variable "sunny" $ Bernoulli 0.4 True False
             return winter
               

tick :: Int -> State Int Int
tick a = do n <- get
            put (n + a)
            return n

submit :: String -> State [String] String
submit s = do l <- get
              put (s:l)
              return s

dessert :: State [String] Int
dessert = do cookie <- submit "cookie"
             milk <- submit "milk"
             chocolate <- submit "chocolate"
             return 3
