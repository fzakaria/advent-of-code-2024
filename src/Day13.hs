module Main where

import Control.Monad (void, foldM, join)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Debug.Trace (trace)
import qualified Data.Map as Map
import qualified Data.Maybe

-- Define a type alias for simplicity
type Parser = Parsec Void String

data Coord = C Int Int
  deriving (Read, Show, Ord, Eq)

add :: Coord -> Coord -> Coord
add (C x y) (C x' y') = C (x + x') (y + y')

sub :: Coord -> Coord -> Coord
sub (C x y) (C x' y') = C (x - x') (y - y')

press:: Coord -> Button -> Coord
press c (Button bc) = sub c bc

newtype Button = Button Coord
  deriving (Read, Show, Ord, Eq)

data Machine
  = Machine
      Button -- A
      Button -- B
      Coord -- Prize
  deriving (Read, Show, Ord, Eq)

integer :: Parser Int
integer = decimal

-- Parse: Button A: X+94, Y+34
button :: Parser Button
button = do
  void $ string "Button "
  void $ string "A: " <|> string "B: "
  x <- string "X+" *> integer
  void $ string ", "
  Button . C x <$> (string "Y+" *> integer)

prize :: Parser Coord
prize = do
  void $ string "Prize: "
  x <- string "X=" *> integer
  void $ string ", "
  C x <$> (string "Y=" *> integer)

machine :: Parser Machine
machine = do
  a <- button
  void newline
  b <- button
  void newline
  Machine a b <$> prize

puzzle :: Parser [Machine]
puzzle = machine `sepBy` some newline

recursiveSolve :: Machine -> Maybe Int
recursiveSolve = snd . go Map.empty
  where
    go :: Map.Map Coord (Maybe Int) -> Machine  -> (Map.Map Coord (Maybe Int), Maybe Int)
    go visited (Machine _ _ (C 0 0)) = (visited, Just 0)
    go visited (Machine a b c)
      | Map.member c visited = (visited, flattenMaybe $ Map.lookup c visited)
      | outOfBounds c = (visited, Nothing)
      | otherwise = let
        visited' = Map.insert c Nothing visited
        (visitedA, resultA) = go visited' (Machine a b (press c a))
        (visitedB, resultB) = go visitedA (Machine a b (press c b))
        bestResult = combineResults resultA resultB
        visitedFinal = Map.insert c bestResult visitedB
        in (visitedFinal, bestResult)

    outOfBounds :: Coord -> Bool
    outOfBounds (C x y) = x < 0 || y < 0

    combineResults :: Maybe Int ->
                      Maybe Int ->
                      Maybe Int
    combineResults Nothing Nothing = Nothing
    combineResults (Just a) Nothing = Just (a + 3)
    combineResults Nothing (Just b) = Just (b + 1)
    combineResults (Just a) (Just b) = Just (min (a + 3) (b + 1))

flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe = join

extendedGCD :: Int -> Int -> (Int, Int, Int)
extendedGCD a 0 = (a, 1, 0)  -- Base case: gcd(a, 0) = a
extendedGCD a b =
  let (g, x1, y1) = extendedGCD b (a `mod` b)  -- Recursive step
  in (g, y1, x1 - (a `div` b) * y1)


-- we basically want to solve
-- a*ax + b*bx = px
-- a*ay + b*by = py
-- These are diophantine equations. We can solve them using the Extended Euclidean Algorithm.
-- They only have a solution if gcd(ax, bx) divides px and gcd(ay, by) divides py.
solve :: Machine -> Maybe Int
solve (Machine (Button (C ax ay)) (Button (C bx by)) (C px py))
  | px `mod` gcdx /= 0 = Nothing
  | py `mod` gcdy /= 0 = Nothing
  | otherwise = Nothing
  where
    gcdx = gcd ax bx
    gcdy = gcd ay by

main :: IO ()
main = do
  input <- readFile "input/Day13.txt"
  case parse puzzle "Day13.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      let answer = sum $ map (Data.Maybe.fromMaybe 0 . recursiveSolve) p
      print answer