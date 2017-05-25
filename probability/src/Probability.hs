{-# LANGUAGE MultiParamTypeClasses #-}
module Probability
  (
    someFunc,
    ProbDist    
  ) where

-- Generic Probability Distribution
-- Maps values to probabilities.
-- Need to create Random Variables
-- Random variables can be Discrete, Continuous
data ProbDist a = ProbDist { }

newtype Normal = Normal mu sigma


class (Num t) => Discrete t where
  mode :: ProbDist t -> Float
  

class (Num t) => Continuous t where
  mode :: ProbDist t -> Float


someFunc :: IO ()
someFunc = putStrLn "someFunc"
