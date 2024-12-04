module Main where

import Data.Void
import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepBy, sepEndBy, some)
import Text.Megaparsec.Char (char, digitChar, newline)

-- Define a type alias for simplicity
type Parser = Parsec Void String

-- Parser for a single integer
integer :: Parser Int
integer = read <$> some digitChar

type Level = [Int]

level :: Parser Level
level = do
  integer `sepBy` char ' '

report :: Parser [Level]
report = level `sepEndBy` newline

diffs :: Level -> [Int]
diffs l = map (uncurry (-)) pairs
  where
    pairs = zip l $ tail l

safe :: [Int] -> Bool
safe d =
  (all (>= 0) d || all (<= 0) d)
    && all (between 1 3) abs_diffs
  where
    abs_diffs = map abs d
    between :: Int -> Int -> Int -> Bool
    between l h n = l <= n && n <= h

alternateLevels :: Level -> [Level]
alternateLevels [] = []
alternateLevels [_] = [[]]
alternateLevels (x:xs) = xs : map (x:) (alternateLevels xs)

-- Main function to read from the file
main :: IO ()
main = do
  input <- readFile "input/Day2.txt"
  case parse report "Day2.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right levels -> do
      print $ length $ filter safe (map diffs levels)
      let allLevels =  map (\l -> l: alternateLevels l) levels
      -- [ [1, 2], [2, 3], [1, 3] , [1, 2, 3] ]
      -- [ [[-1]]  ]
      let allPossibleDiffs =  map (map diffs) allLevels
      print $ length $ filter (any safe) allPossibleDiffs