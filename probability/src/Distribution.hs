{-# LANGUAGE MultiParamTypeClasses #-}
module Distribution where

import qualified Statistics.Distribution as DistLib
import qualified Statistics.Distribution.Normal as NormalLib


-- Generic Distribution Classes  
class Distribution a where
  sum :: a -> Double
  
class Distribution a => Discrete a b where
  -- Probability Mass Function P(X=x)
  pmf :: a -> b -> Double

class Distribution a =>  Continuous a where
  -- Probability Density Function P(X in [x, dx])
  pdf :: a -> Double -> Double
  -- Cumulative Distribution Function P(X < x)
  cdf :: a -> Double -> Double

-- Bernoulli Distribution
data Bernoulli = Bernoulli Double

instance Distribution Bernoulli where
  sum (Bernoulli p) = p + (1 - p)

instance Discrete Bernoulli Double where
  pmf (Bernoulli p) x = if (x == 1) then p else 1 - p

instance Discrete Bernoulli Bool where
  pmf (Bernoulli p) x = if (x == True) then p else 1 - p


-- Normal Distribution
data Normal = Normal Double Double

instance Continuous Normal where
  pdf (Normal mu sigma) = DistLib.density $ NormalLib.normalDistr mu sigma
  cdf (Normal mu sigma) = DistLib.cumulative $ NormalLib.normalDistr mu sigma
  
instance Distribution Normal where
  sum (Normal mu sigma) = (cdf (Normal mu sigma) mu) + (cdf (Normal mu sigma) mu)
  
  
