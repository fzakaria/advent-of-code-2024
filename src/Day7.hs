module Main where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Debug.Trace (trace)

-- Define a type alias for simplicity
type Parser = Parsec Void String

-- Parser for a single integer
integer :: Parser Int
integer = decimal

data Equation = Equation Int [Int]
  deriving (Eq, Show)

equation :: Parser Equation
equation = do
  result <- integer
  _ <- char ':'
  _ <- char ' '
  operands <- integer `sepBy` char ' '
  return $ Equation result operands

puzzle :: Parser [Equation]
puzzle = equation `sepEndBy` newline

possible :: Equation -> Bool
possible (Equation result operands) = go operands 0
  where
    go :: [Int] -> Int -> Bool
    go [] acc = acc == result
    go (x : xs) acc = go xs (acc + x) || go xs (acc * x)

possible' :: Equation -> Bool
possible' (Equation result operands) = go operands 0
  where
    go :: [Int] -> Int -> Bool
    go [] acc = acc == result
    go (x : xs) acc = 
        go xs (combine acc x)||
        go xs (acc + x) ||
        go xs (acc * x)

addDigit :: Int -> Int -> Int
addDigit a b = 10 * a + b

combine :: Int -> Int -> Int
combine a b = read (show a ++ show b)

main :: IO ()
main = do
  input <- readFile "input/Day7.txt"
  case parse puzzle "Day7.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      let possibleResults = filter possible p
      print $ sum $ map (\(Equation r _) -> r) possibleResults
      -- part 2
      let possibleResults' = filter possible' p
      -- print $ possibleResults'
      print $ sum $ map (\(Equation r _) -> r) possibleResults'