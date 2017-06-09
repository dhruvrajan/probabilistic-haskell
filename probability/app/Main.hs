module Main where

import qualified Probability as P

select1 :: P.Distribution String
select1 "Hello, world!" = 0.6
select1 "Howdy, universe!" = 0.4

select2 :: P.Distribution String
select2 "Hello, world!" = 0.2
select2 "Howdy, universe!" = 0.8

main :: IO ()
main = do
  -- Set Up
  let sunnyToday = P.flip 0.2
  let greetingToday1 = P.If sunnyToday (P.fromDistribution select1)
                          (P.fromDistribution select2)
  let sunnyTomorrow = P.If sunnyToday (P.flip 0.8) (P.flip 0.05)
  let greetingTomorrow = P.If sunnyTomorrow (P.fromDistribution select1)
                         (P.fromDistribution select2)

  let prediction = P.probability greetingToday1 "Hello, world!"
  putStrLn("Today's greeting is \"Hello, world!\" " ++
           "with probability " ++ (show prediction) ++ ".")

  let greetingToday2 = P.observe greetingToday1 "Hello, world!"
  let inference = P.probability sunnyToday True
  putStrLn ("If today's greeting is \"Hello, world!\", today's " ++
          "weather is sunny with probability " ++ (show inference) ++ ".")

  let greetingToday3 = P.observe greetingToday2 "Hello, world!"
  let result = P.probability greetingTomorrow "Hello, world!"
  putStrLn("If today's greeting is\"Hello, world!\", " ++
          "tomorrow's greeeting will be \"Hello, world!\" " ++
          "with probability " ++ (show result) ++ ".")
