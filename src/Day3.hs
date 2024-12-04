module Main where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- Define a type alias for simplicity
type Parser = Parsec Void String

-- Parser for a single integer
integer :: Parser Integer
integer = decimal

data Operation
  = Mul Integer Integer
  | Corrupt String
  | Do
  | Dont
  deriving (Eq, Show)

mul :: Parser Operation
mul = do
  void $ string "mul"
  void $ char '('
  a <- integer
  void $ char ','
  b <- integer
  void $ char ')'
  return $ Mul a b

corrupt :: Parser Operation
corrupt = do
  c <- anySingle
  return $ Corrupt [c]

do_ :: Parser Operation
do_ = do
  void $ string "do"
  void $ char '('
  void $ char ')'
  return Do

dont :: Parser Operation
dont = do
  void $ string "don't"
  void $ char '('
  void $ char ')'
  return Dont

memory :: Parser [Operation]
memory = many (choice $ map try [mul, dont, do_, corrupt]) <* eof

filterOperations :: [Operation] -> [Operation]
filterOperations = filterOps False
  where
    filterOps _ [] = []
    filterOps exclude (op : ops) =
      case op of
        Dont -> filterOps True ops
        Do -> filterOps False ops
        Mul _ _ -> if exclude then filterOps True ops else op : filterOps False ops
        Corrupt _ -> filterOps exclude ops

main :: IO ()
main = do
  input <- readFile "input/Day3.txt"
  case parse memory "Day3.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right m -> do
      let mulSum = sum [a * b | Mul a b <- m]
      print mulSum
      let filteredMulSum = sum [a * b | Mul a b <- filterOperations m]
      print filteredMulSum