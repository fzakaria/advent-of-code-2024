module Main where

import Data.Char (digitToInt)
import Data.List (findIndices, group)
import Data.Void (Void)
import qualified Data.Vector as V

import Text.Megaparsec
import Text.Megaparsec.Char (digitChar)

-- Define a type alias for simplicity
type Parser = Parsec Void String

data Block = File Int | Free
  deriving (Eq)

instance Show Block where
  show Free = "."
  show (File i) = show i

-- just like https://hackage.haskell.org/package/megaparsec-4.0.0/docs/src/Text-Megaparsec-Combinator.html#sepBy
puzzle :: Parser [Int]
puzzle = map digitToInt <$> ((:) <$> digitChar <*> many digitChar)

-- Create a disk map from the puzzle
diskMap :: [Int] -> [Block]
diskMap = go 0
  where
    go :: Int -> [Int] -> [Block]
    go _ [] = []
    go i (x : y : xs) =
      replicate x (File i)
        ++ replicate y Free
        ++ go (i + 1) xs
    go i [x] = replicate x (File i)

printDiskMap :: [Block] -> IO ()
printDiskMap d = putStrLn $ foldl (\acc t -> acc ++ show t) "" d

isFile :: Block -> Bool
isFile (File _) = True
isFile _ = False

isFree :: Block -> Bool
isFree b = not $ isFile b

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

swapVector :: Int -> Int -> V.Vector a -> V.Vector a
swapVector i j vec =
  vec V.// [(i, vec V.! j), (j, vec V.! i)]

needsDefrag :: [Block] -> Bool
needsDefrag bs = length (filter (all (== Free)) $ group bs) > 1

ids :: [Block] -> [Int]
ids ((File i) : bs) = i : ids bs
ids (Free : bs) = ids bs
ids [] = []

defrag' :: [Block] -> [Block]
defrag' bs = V.toList $ last $ take n 
                  $ iterate (\vec' -> 
                        swapVector (V.head (V.findIndices isFree vec')) (V.last (V.findIndices isFile vec')) vec'
                    ) vec
    where
        n = length (findIndices isFree bs) - 1
        vec = V.fromList bs 

defrag :: [Block] -> [Block]
defrag [] = []
defrag bs = go
  where
    go
      | needsDefrag bs = defrag (swap (head empties) (last files) bs)
      | otherwise = bs
    files = findIndices isFile bs
    empties = findIndices isFree bs

main :: IO ()
main = do
  input <- readFile "input/Day9.txt"
  case parse puzzle "Day9.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      print "Turning it into disk map"
      let dm = diskMap p
      -- printDiskMap dm
      print "Defragging"
      let defragged = defrag' dm
      -- printDiskMap defragged
      print "Calculating answer"
      let files = ids defragged
      let answer = sum $ zipWith (*) [0::Int ..] files
      print answer
