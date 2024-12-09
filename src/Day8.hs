module Main where

import Data.List
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, newline)

-- Define a type alias for simplicity
type Parser = Parsec Void String

data Tile = Empty | Antenna Char | Antinode
  deriving (Eq, Ord)

instance Show Tile where
  show Empty = "."
  show (Antenna c) = [c]
  show Antinode = "#"

tile :: Parser Tile
tile = choice [Empty <$ char '.', Antenna <$> alphaNumChar]

row :: Parser [Tile]
row = many tile

puzzle :: Parser [[Tile]]
puzzle = row `sepBy` newline

isAntenna :: Tile -> Bool
isAntenna (Antenna _) = True
isAntenna _ = False

-- write Show for puzzle
printPuzzle :: [[Tile]] -> IO ()
printPuzzle [] = return ()
printPuzzle (x : xs) = do
  putStrLn $ foldl (\acc t -> acc ++ show t) "" x
  printPuzzle xs

-- (1, 2) to (3, 1) = 2, -1
-- (3, 1) to (1, 2) = -2, 1

-- (4, 3) to (5, 5) = -1, -2
-- (5, 5) to (4, 3) = 1, 2
allPairs :: [a] -> [(a, a)]
allPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

distance :: (Int, Int) -> (Int, Int) -> (Int, Int)
distance (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sub :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line start vector = iterate (add vector) start

outOfBounds :: Int -> Int -> (Int, Int) -> Bool
outOfBounds rows cols (x, y) = x < 0 || x >= cols || y < 0 || y  >= rows

negative :: (Int, Int) -> (Int, Int)
negative (x, y) = (-x, -y)

main :: IO ()
main = do
  input <- readFile "input/Day8.txt"
  case parse puzzle "Day8.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      printPuzzle p
      -- zip each tile with it's coordinates
      let tiles =
            concat $
              zipWith
                ( \y ys ->
                    zipWith
                      ( \x t ->
                          ((x, y), t)
                      )
                      [0 ..]
                      ys
                )
                [0 ..]
                p
      let antennas = filter (\((_, _), t) -> isAntenna t) tiles
      let sortedAntennas = sortOn snd antennas
      let anntenasByGroup = groupBy (\(_, a) (_, b) -> a == b) sortedAntennas
      let points = map (map fst) anntenasByGroup
      -- now for each group
      let result =
            map
            (\g ->
                let allP = allPairs g
                in
                map (\(a, b) -> let d = distance a b in [ (add d b), (sub a d)]) allP
            ) points
      let flattened = concat $ concat result
      let rows = length p
      let cols = if null p then 0 else length (head p)
      let validPoints = filter (\(x, y) -> x >= 0 && x < cols && y >= 0 && y < rows) flattened
      let uniquePoints = nub validPoints
      print $ length uniquePoints
      -- Part 2
      let vectors =
            concatMap (\g ->
                let allP = allPairs g
                in
                map (\(a, b) -> (a, distance a b)) allP
            ) points

      let l = concatMap (\(a, d) -> takeWhile (not . outOfBounds rows cols) (line a d)) vectors
      let r = concatMap (\(a, d) -> takeWhile (not . outOfBounds rows cols) (line a (negative d))) vectors
      print l
      print r
      print $ length $ nub (l ++ r)