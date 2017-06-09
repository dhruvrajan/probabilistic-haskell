module Probability where

type Distribution a = a -> Double

data Element a = Atomic { distribution :: Distribution a, observed :: Maybe a}
  | If {tst :: Element Bool, thn :: Element a, els :: Element a}

fromDistribution :: Distribution a -> Element a
fromDistribution d = Atomic {distribution = d, observed = Nothing}

flip :: Double -> Element Bool
flip p = fromDistribution $ \x  -> case x of True ->  p
                                             False -> 1 - p
         
observe :: Element a -> a -> Element a
observe e value = e { observed = Just value }

unobserve :: Element a -> Element a
unobserve e = e { observed = Nothing }

probability :: Eq a => Element a -> a -> Double
probability Atomic { distribution = d, observed = Nothing } val = d val
probability Atomic { distribution = d, observed = Just val} x
  = if (x == val) then 1.0 else 0.0
probability If {tst = tst, thn = thn, els = els} val =
  (probability tst True) * (probability thn val) +
  (probability tst False) * (probability els val)

