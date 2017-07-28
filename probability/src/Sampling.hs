{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Sampling where

import Distribution
import System.Random

class Distribution a => Sampler a where
  sample :: 

-- class Discrete a b => Sampler a b where
--   sample :: a -> IO b

-- instance Sampler (Bernoulli Bool) Bool where
--    sample (Bernoulli p s f) = do
--      n <- randomIO :: IO Double
--      return $ if (n < p) then s else f
