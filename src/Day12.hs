module Main where

import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, upperChar)

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

neighbors :: Coord -> [Coord]
neighbors coord = [above coord, below coord, left coord, right coord]

-- Parser for a single integer
garden :: Parser Char
garden = upperChar

row :: Parser [Char]
row = many garden

puzzle :: Parser [[Char]]
puzzle = row `sepBy` newline

outOfBounds :: Coord -> V.Vector (V.Vector Char) -> Bool
outOfBounds (C x y) g = x < 0 || y < 0 || y >= length g || x >= length (V.head g)

value :: V.Vector (V.Vector Char) -> Coord -> Char
value g (C x y) = (g V.! y) V.! x

allCoords :: V.Vector (V.Vector Char) -> [Coord]
allCoords g = [C x y | y <- [0 .. length g - 1], x <- [0 .. length (V.head g) - 1]]

flood :: S.Set Coord -> V.Vector (V.Vector Char) -> Coord -> [Coord]
flood visited g c = snd $ go visited g c
  where
    go :: S.Set Coord -> V.Vector (V.Vector Char) -> Coord -> (S.Set Coord, [Coord])
    go visited g c
      | S.member c visited = (visited, [])
      | outOfBounds c g = (visited, [])
      | otherwise =
          foldl
            ( \(v, acc) n ->
                let (v', result) = go v g n
                 in (v', result ++ acc)
            )
            (newSet, [c])
            nextCoords
      where
        newSet = S.insert c visited
        currentValue = value g c
        nextCoords =
          [ neighbor
            | neighbor <- neighbors c,
              neighbor `S.notMember` visited,
              not $ outOfBounds neighbor g,
              value g neighbor == currentValue
          ]

floodAll :: V.Vector (V.Vector Char) -> [[Coord]]
floodAll g =
  filter (not . null) $
    snd $
      foldl
        ( \(s, rs) c ->
            let r = flood s g c
             in (s `S.union` S.fromList r, r : rs)
        )
        (S.empty, [])
        (allCoords g)

-- The area of a region is simply the number of garden plots the region contains.
area :: [Coord] -> Int
area = length

-- The perimeter of a region is the number of sides of garden plots in the
-- region that do not touch another garden plot in the same region.
perimeter :: V.Vector (V.Vector Char) -> [Coord] -> Int
perimeter _ [] = 0
perimeter g (c : cs) = sum ([1 | neighbor <- neighbors c, outOfBounds neighbor g || value g neighbor /= value g c]) + perimeter g cs

draw :: Int -> Int -> [Coord] -> [[Char]]
draw width height coords = [[if C x y `elem` coords then 'X' else '.' | x <- [0 .. width]] | y <- [0 .. height]]

print' :: [[Char]] -> IO ()
print' g = putStrLn $ unlines g

listTo2DVector :: [[a]] -> V.Vector (V.Vector a)
listTo2DVector = V.fromList . map V.fromList

main :: IO ()
main = do
  input <- readFile "input/Day12.txt"
  case parse puzzle "Day12.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      let v = listTo2DVector p
      -- mapM_ (print' . draw (length (head p) - 1) (length p - 1)) (floodAll v)
      -- print (flood S.empty v (C 0 1))
      print $ sum $ map (\r -> perimeter v r * area r) (floodAll v)

-- print' $ draw (flood p (C 0 0))