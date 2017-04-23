module Main where
import Text.ParserCombinators.Parsec
import Text.CSV
import Data.Either
import Data.Maybe




process :: (Either ParseError CSV) -> Maybe CSV;
process (Right csv) = Just csv
process (Left _) = Nothing

main :: IO ()
main = do x1 <- parseCSVFromFile "../data/csv_test.csv"
          let valid = process x1
          putStrLn (if isJust valid then "True" else "False")

