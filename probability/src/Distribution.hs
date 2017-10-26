{-# LANGUAGE ExistentialQuantification #-}

module Distribution where

import qualified Data.List as ListLib
import qualified Statistics.Distribution as DistLib
import qualified Statistics.Distribution.Normal as NormalLib
import GHC.Float

-- | Class to represent discrete distributions.  Each discrete
-- distribution is defined by its probability mass function
-- (pmf). Additionally, the domains of discrete distributions can be
-- enumerated.
class Discrete a where
  -- Probability Mass Function (PMF)
  pmf :: Eq b => a b -> b -> Double
  -- Enumerate a discrete domain
  domain :: (Eq b) => a b -> [b]
  -- Function to test membership of a value in a discrete domain
  inDomain :: (Eq b) => a b -> b ->  Bool

-- | Class to represent continuous distributions. A continuous
-- distribution is defined by its probability density function (pdf),
-- and its cumulative distribution function (cdf)
class Continuous a where
  -- Probability Density Function (PDF)
  pdf :: (RealFloat b, Fractional b) => a b -> b -> Double
  -- Cumulative Distribution Function (CDF)
  cdf :: (RealFloat b, Fractional b) => a b -> b -> Double

-- Bernoulli Distribution
data Bernoulli a = Bernoulli Double a a

instance Discrete Bernoulli where
  pmf (Bernoulli p s f) x = if (x == s) then p else (if (x == f) then 1 - p else 0.0)
  domain (Bernoulli _ s f)  = [s, f]
  inDomain (Bernoulli _ s f) val = val == s || val == f

-- Select Distribution
data Select a = Select [(a, Double)]

instance Discrete Select where
  pmf (Select pairs) x = case ListLib.lookup x pairs of
                           (Just p) -> p
                           Nothing -> 0                           
  domain (Select pairs) = map fst pairs
  inDomain select val = ListLib.any (\x -> x == val) $ domain select

-- Constant Distribution
data Constant a = Constant a

instance Discrete Constant where
  pmf (Constant c) x = if (c == x) then 1.0 else 0.0
  domain (Constant c) = [c]
  inDomain (Constant c) val = val == c
  
-- Normal Distribution
data Normal a = Normal a a

instance Continuous Normal where
  pdf (Normal mu sigma) x = DistLib.density (NormalLib.normalDistr (realToFrac mu) (realToFrac sigma)) (realToFrac x)
  cdf (Normal mu sigma) x = DistLib.cumulative (NormalLib.normalDistr (realToFrac mu) (realToFrac sigma)) (realToFrac x)
