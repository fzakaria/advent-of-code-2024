module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (newline)
import Data.List (isPrefixOf)

-- Define a type alias for simplicity
type Parser = Parsec Void String

-- Parser for a single letter either X, M, A, or S
letter :: Parser Char
letter = oneOf "XMAS"

-- Parser for a single row of letters until a newline
row :: Parser [Char]
row = many letter

-- Parser for the entire puzzle
puzzle :: Parser [[Char]]
puzzle = row `sepBy` newline

data Direction = Up | Down | L | R | UpLeft | UpRight | DownLeft | DownRight deriving (Eq, Show)

xmasOccurrences :: [[Char]] -> Integer
xmasOccurrences grid = sum [helper (x, y) "" grid dir | (x, y) <- allStartingPoints, dir <- allDirections]
    where
        rows = length grid
        cols = if null grid then 0 else length (head grid)
        allStartingPoints = [(x, y) | x <- [0..rows - 1], y <- [0..cols - 1], grid !! x !! y == 'X']
        -- All possible directions
        allDirections = [Up, Down, L, R, UpLeft, UpRight, DownLeft, DownRight]

        helper :: (Int, Int) -> String -> [[Char]] -> Direction -> Integer
        helper _ "XMAS" _ _ = 1
        helper (x, y) w p dir
            | x < 0 || y < 0 || x >= length p || y >= length (p !! x) = 0
            -- if the current word isn't a prefix of XMAS, we can't continue
            | not (w `isPrefixOf` "XMAS") = 0
            | otherwise =
            let char = p !! x !! y
                w' = w ++ [char]
                (dx, dy) = directionDelta dir
            in helper (x + dx, y + dy) w' p dir
        -- Compute (dx, dy) for each direction
        directionDelta :: Direction -> (Int, Int)
        directionDelta Up        = (-1, 0)
        directionDelta Down      = (1, 0)
        directionDelta L      = (0, -1)
        directionDelta R     = (0, 1)
        directionDelta UpLeft    = (-1, -1)
        directionDelta UpRight   = (-1, 1)
        directionDelta DownLeft  = (1, -1)
        directionDelta DownRight = (1, 1)
main :: IO ()
main = do
  input <- readFile "input/Day4.txt"
  case parse puzzle "Day4.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      print $ xmasOccurrences p