module Main where

import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (minimumBy)
import Data.Ord (comparing)

type Parser = Parsec Void String

data Tile = Empty | Corrupted | Path
  deriving (Read, Ord, Eq)

instance Show Tile where
  show :: Tile -> String
  show Empty = "."
  show Corrupted = "#"
  show Path = "O"

data Coord = C Int Int
  deriving (Read, Show, Ord, Eq)

x :: Coord -> Int
x (C x _) = x

y :: Coord -> Int
y (C _ y) = y

tile :: Parser Coord
tile = do
  x <- decimal
  _ <- char ','
  C x <$> decimal

tiles :: Parser [Coord]
tiles = tile `sepBy` newline

toMap :: Int -> Int -> [Coord] -> M.Map Coord Tile
toMap rows cols ts = do
  let allCoords = [C r c | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
  let emptyMap = M.fromList [(c, Empty) | c <- allCoords]
  let corruptedMap = M.fromList [(c, Corrupted) | c <- ts]
  M.union corruptedMap emptyMap

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = take n xs : split n (drop n xs)

data Distance = Dist Int | Infinity
  deriving (Show, Eq)

instance Ord Distance where
  (<=) :: Distance -> Distance -> Bool
  Infinity <= Infinity = True
  Infinity <= Dist _ = False
  Dist _ <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: Distance -> Distance -> Distance
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

selectMinVertex :: [Coord] -> M.Map Coord Distance -> Coord
selectMinVertex q dist =
  fst $ minimumBy (comparing snd) filtered
  where
    filtered = [(k, v) | k <- q, Just v <- [M.lookup k dist]]

neighbors :: Coord -> [Coord]
neighbors (C x y) = [C (x + 1) y, C (x - 1) y, C x (y + 1), C x (y - 1)]

djikstra :: M.Map Coord Tile -> M.Map Coord Coord
djikstra m = go (M.keys m) initDist M.empty
    where
        go [] _ prev = prev
        go q dist prev = go q' dist' prev'
            where
                u = selectMinVertex q dist
                q' = filter (/= u) q
                neighbors' = [ v | v <- neighbors u, v `elem` q, M.findWithDefault Empty v m /= Corrupted]
                alt = addDist (dist M.! u) (Dist 1)
                prev' =  foldl (\acc v ->
                    if alt < fromMaybe Infinity (M.lookup v dist)
                    then M.insert v u acc
                    else acc) prev neighbors'
                dist' =  foldl (\acc v ->
                    if alt < fromMaybe Infinity (M.lookup v dist)
                    then M.insert v alt acc
                    else acc) dist neighbors'
        initDist = M.fromList [(c, if c == source then Dist 0 else Infinity) | c <- M.keys m]
        source = C 0 0

path :: M.Map Coord Coord -> Coord -> [Coord]
path prev c = go c []
    where
        go c acc = case M.lookup c prev of
            Nothing -> acc
            Just c' -> go c' (c : acc)

printMap :: M.Map Coord Tile -> [Coord] -> IO ()
printMap tileMap path = do
  let allCoords = M.keys tileMap
      rows = maximum (map y allCoords) + 1
      cols = maximum (map x allCoords) + 1
      fullGrid = [C c r | r <- [0 .. (rows - 1)], c <- [0 .. (cols - 1)]]
      mapWithPaths = M.fromList [(c, Path) | c <- path]
      ts = split cols [M.findWithDefault Empty coord (M.union mapWithPaths tileMap) | coord <- fullGrid]
  mapM_ (putStrLn . foldl (\acc t -> acc ++ show t) "") ts

main :: IO ()
main = do
  input <- readFile "input/Day18.txt"
  let rows = 71
  let cols = 71
  let bytes = 1024
  case parse tiles "Day18.txt" input of
    Prelude.Left err -> putStrLn $ errorBundlePretty err
    Prelude.Right ts -> do
      let nts = take bytes ts
      let m = toMap rows cols nts
      let d = djikstra m
      let p = path d (C (rows - 1) (cols - 1))
      printMap m p
      print $ length p
