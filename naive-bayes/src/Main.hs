module Main where
import Text.ParserCombinators.Parsec
import Text.CSV
import Data.Either
import Data.Maybe


-- Statistics Helper Functions
mean :: [Float] -> Float
mean ns = (sum ns) / (fromIntegral (length ns) :: Float)

stdev :: [Float] -> Float
stdev ns = result
  where
    avg = mean(ns)
    variance = (sum [(n - avg)^2 | n <- ns]) / (fromIntegral (length ns - 1) :: Float)
    result = sqrt variance

stringToFloat :: String -> Float
stringToFloat s = read s :: Float

getClassVecs :: CSV -> [([Float], Float)]
getClassVecs [] = []
getClassVecs (x:xs) = (map stringToFloat $ take (length x - 1) x, stringToFloat $ last x) : getClassVecs xs

processCSV :: CSV -> Bool
processCSV x = True


main :: IO ()
main = do x1 <- parseCSVFromFile "data/csv_test.csv"
          let result = (either (\x ->  False) (processCSV))  x1
          putStrLn (if result then "True" else "False")

