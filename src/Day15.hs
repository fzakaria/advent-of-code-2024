module Main where

import Control.Monad (foldM, join, void)
import Data.List (group, nub, sort)
import Data.Map qualified as Map
import Data.Maybe qualified
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import qualified Data.Bifunctor

-- Define a type alias for simplicity
type Parser = Parsec Void String

data Puzzle = Puzzle [[Tile]] [Move]
  deriving (Read, Show, Ord, Eq)

data Tile = Wall | Robot | Box | Empty
  deriving (Read, Ord, Eq)

instance Show Tile where
  show :: Tile -> String
  show Wall = "#"
  show Robot = "@"
  show Box = "O"
  show Empty = "."

data Move = Up | Down | Left | Right
  deriving (Read, Ord, Eq)

instance Show Move where
  show :: Move -> String
  show Up = "^"
  show Down = "v"
  show Main.Left = "<"
  show Main.Right = ">"

newtype Coord = Coord (Int, Int)
  deriving (Read, Show, Ord, Eq)

type Robot = Coord

type Box = Coord

instance Semigroup Coord where
  (<>) = add

instance Monoid Coord where
  mempty = Coord (0, 0)

add :: Coord -> Coord -> Coord
add (Coord (x, y)) (Coord (x', y')) = Coord (x + x', y + y')

addMove :: Move -> Coord -> Coord
addMove Up c = add c (Coord (0, -1))
addMove Down c = add c (Coord (0, 1))
addMove Main.Left c = add c (Coord (-1, 0))
addMove Main.Right c = add c (Coord (1, 0))

integer :: Parser Int
integer = signed space decimal

tile :: Parser Tile
tile = do
  c <- choice [char '#', char '@', char 'O', char '.']
  case c of
    '#' -> return Wall
    '@' -> return Robot
    'O' -> return Box
    '.' -> return Empty
    _ -> fail "Invalid tile"

row :: Parser [Tile]
row = many tile

move :: Parser Move
move = do
  c <- choice [char '<', char '>', char '^', char 'v']
  case c of
    '<' -> return Main.Left
    '>' -> return Main.Right
    '^' -> return Up
    'v' -> return Down
    _ -> fail "Invalid move"

printMap :: [[Tile]] -> IO ()
printMap [] = return ()
printMap (x : xs) = do
  putStrLn $ foldl (\acc t -> acc ++ show t) "" x
  printMap xs

-- Parser for the entire puzzle
puzzle :: Parser Puzzle
puzzle = do
  rows <- row `endBy` newline
  -- void newline
  moves <- many move `sepBy` newline
  return $ Puzzle rows $ concat moves

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

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x : xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

moveRobot :: Map.Map Coord Tile -> Move -> Map.Map Coord Tile
moveRobot m d
  | nextTile == Wall = m -- Do not move if wall
  | nextTile == Empty = Map.insert robot Empty $ Map.insert nextCoord Robot m
  | nextTile == Box && hasEmpty =
    -- we want to move the nextTiles in reverse order
    -- and fold into a new map
    foldl
      ( \acc (c, t) ->
        let c' = addMove d c
          in
          case t of
            Empty -> acc
            Box -> Map.insert c Empty $ Map.insert c' Box acc
            Wall -> acc
            Robot -> Map.insert c Empty $ Map.insert c' Robot acc
      )
      m
      (reverse nextTiles)
  | otherwise = m
  where
    robot = head $ Map.keys $ Map.filter (== Robot) m
    nextCoords = iterate (addMove d) robot
    nextCoord = nextCoords !! 1
    nextTiles = takeUntil (\(_, t) -> t == Empty || t == Wall) $ map (Data.Bifunctor.second (Data.Maybe.fromMaybe Wall) . (\c -> (c, c `Map.lookup` m))) nextCoords
    hasEmpty = Empty `elem` map snd nextTiles
    nextTile = snd (nextTiles !! 1)

main :: IO ()
main = do
  input <- readFile "input/Day15.txt"
  case parse puzzle "Day15.txt" input of
    Prelude.Left err -> putStrLn $ errorBundlePretty err
    Prelude.Right (Puzzle tiles moves) -> do
      let tileMap = toMap tiles
      let moveN = foldl moveRobot tileMap (moves)
      let answer = sum $ map (\(Coord (x, y)) -> 100 * y + x) (Map.keys (Map.filter (== Box) moveN))
      printMap $ toList moveN
      print answer