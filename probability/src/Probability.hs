module Probability where

type Distribution a = a -> Double

data Element a = Element { distribution :: Distribution a,
                           observed :: Maybe a
                         }

observe :: Element a -> a -> Element a
observe e value = e { observed = Just value }

unobserve :: Element a -> Element a
unobserve e = e { observed = Nothing }

probability :: Eq a => Element a -> a -> Double
probability Element { distribution = d, observed = Nothing } = d
probability Element { distribution = d, observed = Just val}
  = \x -> if (x == val) then 1.0 else 0.0


dist :: Distribution Bool
dist True = 0.2
dist False = 0.8

