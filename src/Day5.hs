module Main where

import Data.List (tails, (\\))
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

-- Define a type alias for simplicity
type Parser = Parsec Void String

-- Parser for a single integer
integer :: Parser Integer
integer = decimal

type Rule = (Integer, Integer)

type Update = [Integer]

data Manual = Manual [Rule] [Update]
  deriving (Eq, Show)

rule :: Parser Rule
rule = do
  lower <- integer
  _ <- char '|'
  upper <- integer
  return (lower, upper)

update :: Parser Update
update = integer `sepBy1` char ','

manual :: Parser Manual
manual = do
  r <- rule `endBy1` newline
  _ <- many newline
  u <- update `sepBy1` newline
  return $ Manual r u

generatePairs :: [a] -> [(a, a)]
generatePairs [] = []
generatePairs (x : xs) = pairWithX x xs ++ generatePairs xs
  where
    pairWithX _ [] = []
    pairWithX x (y : ys) = (x, y) : pairWithX x ys

-- Determine if a given update is correct given the list of rules
isCorrectOrder :: [Rule] -> Update -> Bool
isCorrectOrder rs u = null (generatePairs u \\ rs)

middle :: [a] -> Maybe a
middle [] = Nothing
middle xs = Just (xs !! middleIndex)
  where
    middleIndex = length xs `div` 2

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = case splitAt n ls of
  (a, _ : b) -> a ++ (item : b)
  (a, []) -> a ++ [item]

-- swap two indicies in a list
swap :: Int -> Int -> [a] -> [a]
swap i j xs =
  let temp = xs !! i
      xs' = replaceAtIndex i (xs !! j) xs
  in replaceAtIndex j temp xs'

-- the plan is for each bad update
-- generate all pairs again
-- any pair not in a rule has to be flipped
fixUpdate :: [Rule] -> Update -> Update
fixUpdate rs u = fixUpdate' rs u 0 1
    where
        maybeSwap :: Int -> Int -> Update -> Update
        maybeSwap i j u
            | (u !! i, u !! j) `elem` rs = u
            | otherwise = swap i j u

        fixUpdate' :: [Rule] -> Update -> Int -> Int -> Update
        fixUpdate' rs u i j
            | i >= length u = u
            | j >= length u = fixUpdate' rs u (i + 1) (i + 2)
            | otherwise = fixUpdate' rs (maybeSwap i j u) i (j + 1)

main = do
  input <- readFile "input/Day5.txt"
  case parse manual "Day5.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right (Manual rs us) -> do
      let correctUpdates = filter (isCorrectOrder rs) us
      let ms = map middle correctUpdates
      let summed = sum $ fromMaybe 0 <$> ms
      print summed
      -- Part 2
      let badUpdates = filter (not . isCorrectOrder rs) us
      -- the plan is for each bad update
      -- generate all pairs again
      -- any pair not in a rule has to be flipped
      let flipped = map (fixUpdate rs) badUpdates
      let ms = map middle flipped
      let summed = sum $ fromMaybe 0 <$> ms
      print summed