module Inference where

import Element
import Distribution


--expectation :: Eq a => RV a -> a

-- probability :: (Eq a) => RV a -> a -> Double
-- probability (Unobserved (Atomic dist)) val = (pdf dist) val
-- probability (Unobserved (ScalarAdd rv n)) val = probability rv (val - n)
-- probability (Unobserved (ScalarProduct rv n)) val = probability rv (val / n)
-- probability (Unobserved (RVProduct rv n)) val = 
-- --probability (Obser  ved   (Atomic dist) obs) val = if (obs == val) then 1.0 else 0.0




-- probability (Observed (Atomic dist) val) = dist val
-- probability ()


probability :: Eq a => RV a -> a -> Double
probability (Unobserved (Atomic dist)) val = (pdf dist) val
probability (Unobserved (If tst thn els)) val=
  (probability tst True)  * (probability thn val) +
  (probability tst False) * (probability els val)
probability (Observed (_) x) val = if (x == val) then 1.0 else 0.0


