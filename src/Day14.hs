module Main where

import Control.Monad (foldM, join, void)
import Data.List (group, sort)
import Data.Map qualified as Map
import Data.Maybe qualified
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

-- Define a type alias for simplicity
type Parser = Parsec Void String

data Coord = C Int Int
  deriving (Read, Show, Ord, Eq)

data Robot = Robot
  { position :: Coord,
    velocity :: Coord
  }
  deriving (Read, Show, Ord, Eq)

add :: Coord -> Coord -> Coord
add (C x y) (C x' y') = C (x + x') (y + y')

integer :: Parser Int
integer = signed space decimal

robot :: Parser Robot
robot = do
  void $ string "p="
  x <- integer
  void $ string ","
  y <- integer
  void $ string " "
  void $ string "v="
  x' <- integer
  void $ string ","
  Robot (C x y) . C x' <$> integer

mod' :: Int -> Int -> Coord -> Coord
mod' w h (C x y) = C (x `mod` w) (y `mod` h)

move :: Int -> Int -> Robot -> Robot
move w h (Robot p v) = Robot (mod' w h (p `add` v)) v

second :: Int -> Int -> [Robot] -> [Robot]
second w h = map (move w h)

puzzle :: Parser [Robot]
puzzle = robot `sepBy` some newline

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = take n xs : split n (drop n xs)

printRobots :: Int -> Int -> [Robot] -> IO ()
printRobots w h robots = do
  let robotMap = Map.fromListWith (+) (map ((,1) . position) robots)
  let m' = [maybe "." (show . id) (Map.lookup (C x y) robotMap) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
  mapM_ (putStrLn . concat) (split w m')

data Quadrant = LL | LR | UL | UR | Middle
  deriving (Read, Show, Ord, Eq)

quadrant :: Int -> Int -> Robot -> Quadrant
quadrant w h (Robot (C x y) _)
  | x < w `div` 2 && y < h `div` 2 = UL
  | x > w `div` 2 && y < h `div` 2 = UR
  | x < w `div` 2 && y > h `div` 2 = LL
  | x > w `div` 2 && y > h `div` 2 = LR
  | otherwise = Middle

isMiddle :: Quadrant -> Bool
isMiddle Middle = True
isMiddle _ = False

main :: IO ()
main = do
  input <- readFile "input/Day14.txt"
  case parse puzzle "Day14.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      let w = 101
      let h = 103
      let robots = last $ take 100 $ drop 1 (iterate (second w h) p)
      let a = product $ map length $ group . sort $ filter (not . isMiddle) $ map (quadrant w h) robots
      printRobots w h robots
      print a