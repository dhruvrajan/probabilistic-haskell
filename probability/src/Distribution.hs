{-# LANGUAGE MultiParamTypeClasses #-}
module Distribution where
import Data.List (lookup)
import Data.Maybe
  
class Distribution a where
  sum :: a -> Double
  
class Distribution a => Discrete a b where
  -- Probability Mass Function
  pmf :: a -> b -> Double

class Distribution a =>  Continuous a where
  pdf :: a -> Double -> Double
  cdf :: a -> Double -> Double


-- Bernoulli Distribution
data Bernoulli = Bernoulli Double

instance Distribution Bernoulli where
  sum (Bernoulli p) = p + (1 - p)

instance Discrete Bernoulli Double where
  pmf (Bernoulli p) x = if (x == 1) then p else 1 - p

instance Discrete Bernoulli Bool where
  pmf (Bernoulli p) x = if (x == True) then p else 1 - p
