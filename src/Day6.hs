module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Data.List (nub)

-- Define a type alias for simplicity
type Parser = Parsec Void String

data Tile = Obstruction | Empty (Char)
  deriving (Eq, Show)

-- Parser for a single integer
tile :: Parser Tile
tile =
  choice
    [ Obstruction <$ char '#', -- Use <$ to return a fixed value
      Empty <$> (char '.' <|> char '^') -- Use <$> to pass the matched char into Empty
    ]

row :: Parser [Tile]
row = many tile

puzzle :: Parser [[Tile]]
puzzle = row `sepBy` newline

startTile :: [[Tile]] -> Maybe (Int, Int)
startTile ts = search 0 0 ts
  where
    rows = length ts
    cols = if null ts then 0 else length (head ts)
    search x y ts
      | y >= rows = Nothing
      | x >= cols = search 0 (y + 1) ts
      | ts !! y !! x == Empty '^' = Just (x, y)
      | otherwise = search (x + 1) y ts

data Direction = North | East | South | West
  deriving (Eq, Show)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

step :: Direction -> (Int, Int) -> (Int, Int)
step North (x, y) = (x, y - 1)
step East (x, y) = (x + 1, y)
step South (x, y) = (x, y + 1)
step West (x, y) = (x - 1, y)

tileAt :: [[Tile]] -> (Int, Int) -> Maybe Tile
tileAt ts (x, y)
  | x < 0 || y < 0 || x >= cols || y >= rows = Nothing
  | otherwise = Just $ ts !! y !! x
  where
    rows = length ts
    cols = if null ts then 0 else length (head ts)

stepGuard :: [[Tile]] -> (Direction, (Int, Int)) -> (Direction, (Int, Int))
stepGuard ts (d, (x, y))
  -- if we are at the end we are done
  | x < 0 || y < 0 || x >= cols || y >= rows = (d, (x, y))
  | nextTile == Just Obstruction = (rightDirection, nextPosRight)
  | otherwise = (d, nextPos)
  where
    rows = length ts
    cols = if null ts then 0 else length (head ts)
    rightDirection = turnRight d
    nextPosRight = step rightDirection (x, y)
    nextPos = step d (x, y)
    nextTile = tileAt ts (step d (x, y))

takeUntilConverges :: Eq a => (a -> a) -> a -> [a]
takeUntilConverges f = takeUntil
  where
    takeUntil current
      | next == current = [current]
      | otherwise = current : takeUntil next
      where
        next = f current

main :: IO ()
main = do
  input <- readFile "input/Day6.txt"
  case parse puzzle "Day5.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      case startTile p of
        Nothing -> putStrLn "No start tile found!"
        Just pos -> do
          let steps = takeUntilConverges (stepGuard p) (North, pos)
          -- Remove the last step since it's outside the puzzle
          let withoutLastStep = init steps
          let tiles = map snd withoutLastStep
          print $ length $ nub tiles
