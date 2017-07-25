module Inference where

import Element
import Distribution

probability :: Eq a => RV a -> a -> Double
probability (Unobserved (DiscreteAtomic dist)) val = pmf dist val
probability (Unobserved (ContinuousAtomic dist)) val = pdf dist val
probability (Unobserved (If tst thn els)) val=
  (probability tst True)  * (probability thn val) +
  (probability tst False) * (probability els val)
probability (Observed (_) x) val = if (x == val) then 1.0 else 0.0


