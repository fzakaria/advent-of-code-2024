module Main where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Bits (xor)
import Debug.Trace (trace)

type Parser = Parsec Void String

type Register = Int

data Computer = Computer
  { a :: Register,
    b :: Register,
    c :: Register,
    pc :: Register,
    instructions :: [Instruction],
    output :: [Int]
  }
  deriving (Read, Show, Ord, Eq)

data OpCode = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV
  deriving (Read, Ord, Eq)

makeOpCode :: Int -> OpCode
makeOpCode 0 = ADV
makeOpCode 1 = BXL
makeOpCode 2 = BST
makeOpCode 3 = JNZ
makeOpCode 4 = BXC
makeOpCode 5 = OUT
makeOpCode 6 = BDV
makeOpCode 7 = CDV
makeOpCode _ = error "Invalid OpCode"

instance Show OpCode where
  show :: OpCode -> String
  show ADV = "adv"
  show BXL = "bxl"
  show BST = "bst"
  show JNZ = "jnz"
  show BXC = "bxc"
  show OUT = "out"
  show BDV = "bdv"
  show CDV = "cdv"

newtype Instruction = Instruction (OpCode, Int)
  deriving (Read, Ord, Eq)

instance Show Instruction where
  show :: Instruction -> String
  show (Instruction (op, n)) = show op ++ " " ++ show n

register :: String -> Parser Register
register name = do
  _ <- string "Register "
  _ <- string name
  _ <- string ": "
  decimal

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x : y : xs) = (x, y) : pairs xs

computer :: Parser Computer
computer = do
  a <- register "A" <* newline
  b <- register "B" <* newline
  c <- register "C" <* newline
  _ <- newline
  _ <- string "Program: "
  numbers <- decimal `sepBy` char ','
  let instructions = map (\(x, y) -> Instruction (makeOpCode x, y)) $ pairs numbers
  return $ Computer a b c 0 instructions []

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x : _) 0 = Just x
(!?) (_ : xs) n = (!?) xs (n - 1)

operand :: (Register, Register, Register) -> Instruction -> Int
operand rs (Instruction (ADV, n)) = comboOperand rs n
operand _ (Instruction (BXL, n)) = n
operand rs (Instruction (BST, n)) = comboOperand rs n
operand _ (Instruction (JNZ, n)) = n
operand _ (Instruction (BXC, n)) = n
operand rs (Instruction (OUT, n)) = comboOperand rs n
operand rs (Instruction (BDV, n)) = comboOperand rs n
operand rs (Instruction (CDV, n)) = comboOperand rs n

comboOperand :: (Register, Register, Register) -> Int -> Int
comboOperand _ o | o <= 3 = o
comboOperand (a, _, _) 4 = a
comboOperand (_, b, _) 5 = b
comboOperand (_, _, c) 6 = c
comboOperand _ 7 = error "reserved operand"
comboOperand _ _ = error "invalid operand"

execute ::
  (Register, Register, Register) ->
  Computer ->
  Instruction ->
  Computer
execute rs@(a, _, _) comp i@(Instruction (ADV, _)) = advance comp {a = a `div` 2 ^operand rs i}
execute rs@(_, b, _) comp i@(Instruction (BXL, _)) = advance comp {b = b `xor` operand rs i}
execute rs comp i@(Instruction (BST, _)) = advance comp {b = operand rs i `mod` 8}
execute rs@(a, _, _) comp i@(Instruction (JNZ, _)) = if a == 0 
    then advance comp else
    comp { pc = operand rs i }
execute (_, b, c) comp (Instruction (BXC, _)) = advance comp {b = b `xor`c}
execute rs comp i@(Instruction (OUT, _)) = advance comp {output = output comp ++ [operand rs i `mod` 8] }
execute rs@(a, _, _) comp i@(Instruction (BDV, _)) = advance comp {b = a `div` 2 ^operand rs i}
execute rs@(a, _, _) comp i@(Instruction (CDV, _)) = advance comp {c = a `div` 2 ^operand rs i}

advance :: Computer -> Computer
advance comp = comp {pc = pc comp + 2}

step :: Computer -> Maybe Computer
step comp@(Computer a b c pc instructions _) =
  let i = instructions !? (pc `div` 2)
   in (execute (a, b, c) comp <$> i)

main :: IO ()
main = do
  input <- readFile "input/Day17.txt"
  case parse computer "Day17.txt" input of
    Prelude.Left err -> putStrLn $ errorBundlePretty err
    Prelude.Right c -> do
      let cs = iterate (>>= step) (pure c)
      let c' = last $ takeWhile (/= Nothing) cs
      print (output <$> c')