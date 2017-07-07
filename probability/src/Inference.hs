module Inference where

import Element
import Distribution


--expectation :: Eq a => RV a -> a

probability :: (Eq a) => RV a -> a -> Double
probability (Unobserved (Atomic dist)) val = (pdf dist) val
probability (Unobserved (ScalarAdd rv n)) val = probability rv (val - n)
probability (Unobserved (ScalarProduct rv n)) val = probability rv (val / n)
probability (Unobserved (RVProduct rv n)) val = 
--probability (Observed   (Atomic dist) obs) val = if (obs == val) then 1.0 else 0.0




-- probability (Observed (Atomic dist) val) = dist val
-- probability ()

-- probability :: Eq a => Element a -> a -> Double
-- probability (Atomic dist ) val = dist val
-- probability (Atomic dist (Just x)) val = if (x == val) then 1.0 else 0.0

-- probability (If tst thn els Nothing) val =
--   (probability tst True) * (probability thn val) +
--   (probability tst False) * (probability els val)

