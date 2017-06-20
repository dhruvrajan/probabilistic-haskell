module Monads where

isBigGang :: Int -> Bool
isBigGang x = x > 9

main :: IO ()
main = do
  putStrLn("hey there!")
