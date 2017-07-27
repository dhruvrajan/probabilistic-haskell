module Coin where


data Coin = Head | Tail deriving (Eq, Show)

data Bernoulli a = Bernoulli Double a a

class Distribution a where
  sumProbabilities :: a b -> Double

class Distribution a => Discrete a where
  pmf :: Eq b => a b -> b -> Double

instance Distribution Bernoulli where
    sumProbabilities (Bernoulli p s f) = p + (1 - p)
  
instance Discrete Bernoulli where
  pmf (Bernoulli p s f) x = if (x == s) then p else 1 - p
  


