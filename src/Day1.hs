module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List
import qualified Data.Map.Strict as Map

-- Define a type alias for simplicity
type Parser = Parsec Void String

-- Parser for a single integer
integer :: Parser Int
integer = read <$> some digitChar

-- Parser for a single row
row :: Parser (Int, Int)
row = do
  first <- integer
  space
  second <- integer
  return (first, second)

-- Parser for the entire input
rows :: Parser [(Int, Int)]
rows = row `sepEndBy` newline

-- Function to pair minimum values
pairMins :: [Int] -> [Int] -> [(Int, Int)]
pairMins left right = zip (sort left) (sort right)

calculateWeightedSum :: [Int] -> [Int] -> Int
calculateWeightedSum left right = foldl process 0 left
  where
    -- Create a Map where keys are numbers in `right` and values are their occurrences
    occurrenceMap :: Map.Map Int Int
    occurrenceMap = Map.fromList [(head g, length g) | g <- group (sort right)]

    -- Process each value in `left`
    process :: Int -> Int -> Int
    process acc x =
      case Map.lookup x occurrenceMap of
        Just occurrence -> acc + (x * occurrence) -- Multiply value by its occurrence
        Nothing -> acc -- If not found, add 0

-- Main function to read from the file
main :: IO ()
main = do
  input <- readFile "input/Day1.txt" -- Read the contents of Day1.txt
  case parse rows "Day1.txt" input of
    Left err -> putStrLn $ errorBundlePretty err -- Print parsing errors if any
    Right result -> do
      let (left, right) = unzip result
      let pairs = pairMins left right
      let answer = foldl (\acc (lhs, rhs) -> acc + abs(lhs - rhs)) 0 pairs
      -- part 1
      print answer
      -- part 2
      let answer2 = calculateWeightedSum left right
      print answer2
