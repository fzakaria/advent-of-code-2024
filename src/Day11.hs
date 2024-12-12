module Main where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

-- Define a type alias for simplicity
type Parser = Parsec Void String

-- Parser for a single integer
stone :: Parser Int
stone = decimal

puzzle :: Parser [Int]
puzzle = stone `sepBy` spaceChar

-- Does not handle 0 but we know this rule
-- will never
digits :: Int -> [Int]
digits 0 = [0]
digits x =
  ( if x `div` 10 > 0
      then digits (x `div` 10)
      else []
  )
    ++ [x `mod` 10]

number :: [Int] -> Int
number = foldl (\acc x -> acc * 10 + x) 0

blinkStone :: Int -> [Int]
-- If the stone is engraved with the number 0,
-- it is replaced by a stone engraved with the number 1.
blinkStone 0 = [1]
blinkStone n
  | even' n = [number $ fst split, number $ snd split]
  | otherwise = [2024 * n]
  where
    ds = digits n
    split = splitAt (length ds `div` 2) ds
    even' = even . length . digits :: Int -> Bool

blink :: [Int] -> [Int]
blink = concatMap blinkStone

main :: IO ()
main = do
  input <- readFile "input/Day11.txt"
  case parse puzzle "Day11.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      -- mapM print $ take 6 $ iterate blink p
      -- We need to take one to get the 25th iteration
      print $ length . last $ take 26 $ iterate blink p