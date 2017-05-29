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

data Normal = Normal Double Double

class Probability where
  by :: Int -> Int



class (Num t) => Discrete t where
  mode :: ProbDist t -> Float
  




someFunc :: IO ()
someFunc = putStrLn "someFunc"
