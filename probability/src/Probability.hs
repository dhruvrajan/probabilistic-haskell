{-# LANGUAGE ExistentialQuantification #-}
module Probability where
import Control.Monad

type Distribution a = a -> Double

data Element a =
  Atomic (Distribution a) (Maybe a)
  | If (Element Bool) (Element a) (Element a) (Maybe a)
  | forall b. Apply (Element b) (b -> a) (Maybe a)
  | forall b. Chain (Element b) (b -> Element a) (Maybe a)

observe :: Element a -> a -> Element a
observe (Atomic dist _) obs    = Atomic dist $ Just obs
observe (If tst thn els _) obs = If tst thn els $ Just obs
observe (Apply emt fn _) obs   = Apply emt fn $ Just obs
observe (Chain emt fn _) obs   = Chain emt fn $ Just obs

unobserve :: Element a -> Element a
unobserve (Atomic dist _)    = Atomic dist $ Nothing
unobserve (If tst thn els _) = If tst thn els $ Nothing
unobserve (Apply emt fn _)   = Apply emt fn $ Nothing
unobserve (Chain emt fn _)   = Chain emt fn $ Nothing


probability :: Eq a => Element a -> a -> Double
probability (Atomic dist Nothing) val = dist val
probability (Atomic dist (Just x)) val = if (x == val) then 1.0 else 0.0

probability (If tst thn els Nothing) val =
  (probability tst True) * (probability thn val) +
  (probability tst False) * (probability els val)




