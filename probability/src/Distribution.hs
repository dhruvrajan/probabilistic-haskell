{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution where

import qualified Data.List as ListLib
import qualified Statistics.Distribution as DistLib
import qualified Statistics.Distribution.Normal as NormalLib

-- Generic Distribution Classes  
class Distribution a where
  sumProbabilities :: a -> Double
  
class Distribution a => Discrete a b where
  -- Probability Mass Function P(X=x)
  pmf :: a -> b -> Double

  -- fromDiscrete :: a -> RV b
  -- fromDiscrete dist = Unobserved $ DiscreteAtomic dist

class Distribution a => Continuous a b where
  -- Probability Density Function P(X in [x, dx])
  pdf :: a -> b -> Double
  -- Cumulative Distribution Function P(X < x)
  cdf :: a  -> b -> Double

  -- fromContinuous :: Continuous d -> RV a
  -- fromContinuous dist = Unobserved $ ContinuousAtomic dist


-- Bernoulli Distribution
data Bernoulli = Bernoulli Double

instance Distribution Bernoulli where
  sumProbabilities (Bernoulli p) = p + (1 - p)

instance Discrete Bernoulli Double where
  pmf (Bernoulli p) x = if (x == 1) then p else 1 - p

instance Discrete Bernoulli Bool where
  pmf (Bernoulli p) x = if (x == True) then p else 1 - p

-- Select Distribution
data Select a = Eq a => Select [(a, Double)]

instance Distribution (Select a) where
  sumProbabilities (Select []) = 0
  sumProbabilities (Select ((_, p):pairs)) = p + sumProbabilities (Select pairs)

instance Discrete (Select a) a where
  pmf (Select pairs) x = case ListLib.lookup x pairs of
                           (Just p) -> p
                           Nothing -> 0

-- Normal Distribution
data Normal = Normal Double Double

instance Continuous Normal Double where
  pdf (Normal mu sigma) = DistLib.density $ NormalLib.normalDistr mu sigma
  cdf (Normal mu sigma) = DistLib.cumulative $ NormalLib.normalDistr mu sigma
  
instance Distribution Normal where
  sumProbabilities (Normal mu sigma) = (cdf (Normal mu sigma) mu) + (cdf (Normal mu sigma) mu)
  
  
