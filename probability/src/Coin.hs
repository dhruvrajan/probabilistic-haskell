module Coin where


data Coin = Head | Tail deriving (Eq, Show)

data Bernoulli a = Bernoulli Double a a

class Distribution a where
  sumProbabilities :: a -> Double

instance Distribution (Bernoulli a) where
    sumProbabilities (Bernoulli p s f) = p + (1 - p)

class Discrete a where
  pmf :: Eq b => a b -> b -> Double

instance Discrete Bernoulli where
   pmf (Bernoulli p s f) x = if (x == s) then p else 1 - p
