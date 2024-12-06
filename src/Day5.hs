module Main where

import Data.List (tails, (\\))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Maybe (fromMaybe)

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
isCorrectOrder :: [Rule] -> Update  -> Bool
isCorrectOrder rs u = null (generatePairs u \\ rs)

middle :: [a] -> Maybe a
middle [] = Nothing
middle xs = Just (xs !! middleIndex)
  where
    middleIndex = length xs `div` 2

main :: IO ()
main = do
  input <- readFile "input/Day5.txt"
  case parse manual "Day5.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right (Manual rs us) -> do
      let correctUpdates = filter (isCorrectOrder rs) us
      let ms = map middle correctUpdates
      let summed = sum $ fromMaybe 0 <$> ms
      print summed