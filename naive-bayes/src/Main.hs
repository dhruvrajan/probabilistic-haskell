module Main where
import Text.ParserCombinators.Parsec
import Text.CSV
import Data.Either
import Data.Maybe
import qualified Data.Map as Map


-- Statistics Helper Functions
mean :: [Float] -> Float
mean ns = (sum ns) / (fromIntegral (length ns) :: Float)

stdev :: [Float] -> Float
stdev ns = result
  where
    avg = mean(ns)
    variance = (sum [(n - avg)^2 | n <- ns]) / (fromIntegral (length ns - 1) :: Float)
    result = sqrt variance


-- data transformations
stringToFloat :: String -> Float
stringToFloat s = read s :: Float

getClassVecs :: CSV -> [(Int, [Float])]
getClassVecs [] = []
getClassVecs (x:xs) = (read $ last x :: Int, map stringToFloat $ take (length x - 1) x) : getClassVecs xs


addToClass :: (Int, [Float]) -> [(Int, [[Float]])] -> [(Int, [[Float]])]
addToClass (x, ys) [] = [(x, [ys])]
addToClass (x, ys) ((z1,z2) : zs) = if (x == z1)
                                    then (z1, ys : z2) : zs
                                    else (z1,  z2) : (addToClass (x, ys) zs)
                                                 
    

separateByClass :: [(Int, [Float])] -> [(Int, [[Float]])] -> [(Int, [[Float]])]
separateByClass [] ys = ys
separateByClass (x:xs) ys = separateByClass xs (addToClass x ys)


getCSVData :: IO([Record])
getCSVData = do d <- parseCSVFromFile "data/csv_test.csv"
                let y = head (rights [d])
                return $ take (length y - 1) y




processCSV :: CSV -> Bool
processCSV x = True


main :: IO ()
main = do x1 <- parseCSVFromFile "data/csv_test.csv"
          let result = (either (\x ->  False) (processCSV))  x1
          putStrLn (if result then "True" else "False")

