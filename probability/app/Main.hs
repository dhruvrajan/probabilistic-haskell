module Main where

import Distribution
import Element
import Inference

select1 :: Select String
select1 = Select [("Hello, world!", 0.6), ("Howdy, universe!", 0.4)]

select2 :: Select String
select2 = Select [("Hello, world!", 0.2), ("Howdy, universe!", 0.8)]

main :: IO ()
main  = do
  -- Set Up
  let sunnyToday = Unobserved $ DiscreteAtomic $ Bernoulli 0.2
  let greetingToday1 = Unobserved $ If sunnyToday (Unobserved $ DiscreteAtomic select1) (Unobserved $ DiscreteAtomic select2)

  let sunnyTomorrow = Unobserved $ If sunnyToday (Unobserved $ DiscreteAtomic $ Bernoulli 0.8) (Unobserved $ DiscreteAtomic $ Bernoulli 0.05)
  let greetingTomorrow = Unobserved $ If sunnyTomorrow (Unobserved $ DiscreteAtomic select1) (Unobserved $ DiscreteAtomic  select2)

  -- Predict
  let prediction = probability greetingToday1 "Hello, world!"
  putStrLn("Today's greeting is \"Hello, world!\" " ++
           "with probability " ++ (show prediction) ++ ".")

  -- Infer
  let greetingToday2 = observe greetingToday1 "Hello, world!"
  let inference = probability sunnyToday True
  putStrLn ("If today's greeting is \"Hello, world!\", today's " ++
            "weather is sunny with probability " ++ (show inference) ++ ".")

  -- Learn And Predict
  let greetingToday3 = observe greetingToday2 "Hello, world!"
  let result = probability greetingTomorrow "Hello, world!"
  putStrLn("If today's greeting is\"Hello, world!\", " ++
          "tomorrow's greeeting will be \"Hello, world!\" " ++
          "with probability " ++ (show result) ++ ".")

  putStrLn("Finished.")
  
  -- let greetingToday1 = If sunnyToday (fromDistribution select1)
  --                         (fromDistribution select2) Nothing
  -- let sunnyTomorrow = If sunnyToday (flip 0.8) (flip 0.05) Nothing
  -- let greetingTomorrow = If sunnyTomorrow (fromDistribution select1)
  --                        (fromDistribution select2) Nothing
  -- -- Predict
  -- let prediction = probability greetingToday1 "Hello, world!"
  -- putStrLn("Today's greeting is \"Hello, world!\" " ++
  --          "with probability " ++ (show prediction) ++ ".")

  -- -- Infer
  -- let greetingToday2 = observe greetingToday1 "Hello, world!"
  -- let inference = probability sunnyToday True
  -- putStrLn ("If today's greeting is \"Hello, world!\", today's " ++
  --         "weather is sunny with probability " ++ (show inference) ++ ".")

  -- -- Learn And Predict
  -- let greetingToday3 = observe greetingToday2 "Hello, world!"
  -- let result = probability greetingTomorrow "Hello, world!"
  -- putStrLn("If today's greeting is\"Hello, world!\", " ++
  --         "tomorrow's greeeting will be \"Hello, world!\" " ++
  --         "with probability " ++ (show result) ++ ".")
