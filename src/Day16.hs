{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module Main where

import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Maybe qualified
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)

-- Define a type alias for simplicity
type Parser = Parsec Void String

newtype Puzzle = Puzzle [[Tile]]
  deriving (Read, Show, Ord, Eq)

data Tile = Start | End | Wall | Empty
  deriving (Read, Ord, Eq)

instance Show Tile where
  show :: Tile -> String
  show Wall = "#"
  show Empty = "."
  show Start = "S"
  show End = "E"

data Direction = North | South | East | West
  deriving (Read, Ord, Eq)

instance Show Direction where
  show :: Direction -> String
  show North = "^"
  show South = "v"
  show East = ">"
  show West = "<"

newtype Coord = Coord (Int, Int)
  deriving (Read, Show, Ord, Eq)

type Position = (Coord, Direction)

move :: Position -> Position
move ((Coord (x, y)), North) = (Coord (x, y - 1), North)
move ((Coord (x, y)), South) = (Coord (x, y + 1), South)
move ((Coord (x, y)), East) = (Coord (x + 1, y), East)
move ((Coord (x, y)), West) = (Coord (x - 1, y), West)

clockwise :: Direction -> Direction
clockwise North = East
clockwise East = South
clockwise South = West
clockwise West = North

counterClockwise :: Direction -> Direction
counterClockwise North = West
counterClockwise West = South
counterClockwise South = East
counterClockwise East = North

tile :: Parser Tile
tile = do
  c <-
    choice
      [ char '#',
        char 'S',
        char 'E',
        char '.'
      ]
  case c of
    '#' -> return Wall
    'S' -> return Start
    'E' -> return End
    '.' -> return Empty
    _ -> fail "Invalid tile"

row :: Parser [Tile]
row = many tile

printMap :: [[Tile]] -> IO ()
printMap [] = return ()
printMap (x : xs) = do
  putStrLn $ foldl (\acc t -> acc ++ show t) "" x
  printMap xs

-- Parser for the entire puzzle
puzzle :: Parser Puzzle
puzzle = do
  rows <- row `sepBy1` newline
  return (Puzzle rows)

toMap :: [[Tile]] -> Map.Map Coord Tile
toMap tiles = Map.fromList $ do
  (y, r) <- zip [0 ..] tiles
  (x, t) <- zip [0 ..] r
  return (Coord (x, y), t)

toList :: Map.Map Coord Tile -> [[Tile]]
toList m = do
  y <- [0 .. maxY]
  return $ do
    x <- [0 .. maxX]
    return $ Data.Maybe.fromMaybe Empty $ Map.lookup (Coord (x, y)) m
  where
    keys = Map.keys m
    xs = map (\(Coord (x, _)) -> x) keys
    ys = map (\(Coord (_, y)) -> y) keys
    maxX = maximum xs
    maxY = maximum ys

outOfBounds :: Coord -> Map.Map Coord Tile -> Bool
outOfBounds (Coord (x, y)) m = x < 0 || y < 0 || x > maxX || y > maxY
  where
    keys = Map.keys m
    xs = map (\(Coord (x, _)) -> x) keys
    ys = map (\(Coord (_, y)) -> y) keys
    maxX = maximum xs
    maxY = maximum ys

-- Function to find the smallest element
smallestElement :: [Maybe (S.Set (Coord, Direction), Int)] -> Maybe (S.Set (Coord, Direction), Int)
smallestElement list =
  case catMaybes list of
    [] -> Nothing
    justList -> Just $ minimumBy (comparing snd) justList

minOfTwoMaps :: Map.Map Position Int -> Map.Map Position Int -> Map.Map Position Int
minOfTwoMaps = Map.unionWith min

-- Given a move return the possible moves we can make and the cost
possibleMoves :: Position -> [(Position, Int)]
possibleMoves p@(c, d) = (move p, 1) : [((c, dir d), 1000) | dir <- [clockwise, counterClockwise]]

solve :: Map.Map Coord Tile -> Map.Map Position Int
solve m = go (startPos, East) Map.empty 0
  where
    go :: Position -> Map.Map Position Int -> Int -> Map.Map Position Int
    go p@(c, d) v cost
      -- Stop if hitting a wall
      | Map.lookup c m == Just Wall = v
      -- Goal reached
      -- \| c == endPos = Map.insert p cost v
      -- Stop if already visited
      -- \| Map.member p v = if cost < v Map.! p then Map.insert p cost v else v
      | otherwise =
          foldl
            ( \acc (p', c') ->
                let cost' = cost + c'
                 in newMap (p', cost') acc
            )
            v
            (possibleMoves p)
      where
        newMap :: (Position, Int) -> Map.Map Position Int -> Map.Map Position Int
        newMap (p', c') v' = if not (Map.member p' v') || c' < v' Map.! p' then (go p' (Map.insert p' c' v') c') else v'
    startPos = head $ Map.keys $ Map.filter (== Start) m
    endPos = head $ Map.keys $ Map.filter (== End) m

main :: IO ()
main = do
  input <- readFile "input/Day16.txt"
  case parse puzzle "Day16.txt" input of
    Prelude.Left err -> putStrLn $ errorBundlePretty err
    Prelude.Right (Puzzle tiles) -> do
      let tileMap = toMap tiles
      printMap tiles
      let solutions = solve tileMap
      -- print solutions
      --let startPos = head $ Map.keys $ Map.filter (== Start) tileMap
      let endPos = head $ Map.keys $ Map.filter (== End) tileMap
     -- print $ solutions Map.! (startPos, East)
      print $ minimumBy (comparing snd) $ Map.toList $ Map.filterWithKey  (\(c, _) _ -> c == endPos) solutions