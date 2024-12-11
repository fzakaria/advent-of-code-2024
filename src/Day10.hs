module Main where

import Data.Char (digitToInt)
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, newline)

-- Define a type alias for simplicity
type Parser = Parsec Void String

data Coord = C Int Int
  deriving (Read, Show, Ord, Eq)

-- | Decrement y coordinate
above :: Coord -> Coord
above (C x y) = C x (y - 1)

-- | Increment y coordinate
below :: Coord -> Coord
below (C x y) = C x (y + 1)

-- | Decrement x coordinate
left :: Coord -> Coord
left (C x y) = C (x - 1) y

-- | Increment x coordinate
right :: Coord -> Coord
right (C x y) = C (x + 1) y

-- just like https://hackage.haskell.org/package/megaparsec-4.0.0/docs/src/Text-Megaparsec-Combinator.html#sepBy
row :: Parser [Int]
row = map digitToInt <$> ((:) <$> digitChar <*> many digitChar)

puzzle :: Parser [[Int]]
puzzle = row `sepBy` newline

isTrailhead :: Int -> Bool
isTrailhead = (==) 0

isPeak :: Int -> Bool
isPeak = (==) 9

trailheads :: [[Int]] -> [Coord]
trailheads = go 0 0
  where
    go :: Int -> Int -> [[Int]] -> [Coord]
    go x y ts
      | y >= rows = []
      | x >= cols = go 0 (y + 1) ts
      | isTrailhead $ ts !! y !! x = C x y : go (x + 1) y ts
      | otherwise = go (x + 1) y ts
      where
        rows = length ts
        cols = if null ts then 0 else length (head ts)

-- Replace the concatenation with finding the longest list
longestTrail :: (Ord a) => [[a]] -> [a]
longestTrail = maximumBy (comparing length)

trails ::
  [[Int]] -> -- map
  Coord -> -- trailhead
  [[Coord]]
trails m c@(C x y)
  -- out of bounds, return empty
  | outOfBounds (x, y) = []
  -- we are done recurse out
  | isPeak $ m !! y !! x = [[c]]
  | otherwise =
      map (c : ) $
          concat [ if canStepTo (above c) then trails m (above c) else [],
            if canStepTo (below c) then trails m (below c) else [],
            if canStepTo (left c) then trails m (left c) else [],
            if canStepTo (right c) then trails m (right c) else []
          ]
  where
    current = m !! y !! x
    rows = length m
    cols = if null m then 0 else length (head m)
    -- Check if a step is valid
    canStepTo (C nx ny) =
      not (outOfBounds (nx, ny)) && (m !! ny !! nx == current + 1)
    outOfBounds (x, y) = x < 0 || y < 0 || x >= cols || y >= rows

printTrail :: [Coord] -> -- trail
              [[Int]] -> -- map
              IO ()
printTrail = go 0 0
  where
    go :: Int -> Int -> [Coord] -> [[Int]] -> IO ()
    go x y t m
      | y >= rows = putStrLn ""
      | x >= cols = do
        putStrLn ""
        go 0 (y + 1) t m
      | C x y `elem` t = do
        putStr (show (m !! y !! x))
        go (x + 1) y t m
      | otherwise = do
        putStr "."
        go (x + 1) y t m
      where
        rows = length m
        cols = if null m then 0 else length (head m)

main :: IO ()
main = do
  input <- readFile "input/Day10.txt"
  case parse puzzle "Day10.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      let ths = trailheads p
      -- sequence_ $ mapM printTrail (trails (head ths) p) p
      -- this is a list of lists of trails
      let a = map (trails p) ths
      let peaks =  map (map last) a
      let uniquePeaks = map nub peaks
      let score = sum $ map length uniquePeaks
      print score