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

data CompactBlock = CompactFile {id :: Int, size :: Int} | CompactFree { size :: Int}
  deriving (Eq)

instance Show Block where
  show Free = "."
  show (File i) = show i

instance Show CompactBlock where
  show (CompactFree size) = replicate size '.'
  show (CompactFile i size) = foldl (\acc t -> acc ++ show t) "" (replicate size i)

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

printCompactDiskMap :: [CompactBlock] -> IO ()
printCompactDiskMap d = putStrLn $ foldl (\acc t -> acc ++ show t) "" d

isCompactFile :: CompactBlock -> Bool
isCompactFile (CompactFile _ _) = True
isCompactFile _ = False

isCompactEmpty :: CompactBlock -> Bool
isCompactEmpty b = not $ isCompactFile b

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
ids (Free : bs) = 0 : ids bs
ids [] = []

unpack :: [CompactBlock] -> [Block]
unpack ((CompactFile i size) : bs) = replicate size (File i) ++ unpack bs
unpack ((CompactFree size) : bs) = replicate size Free ++ unpack bs
unpack [] = []

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

defragStep :: (Int, [CompactBlock]) -> (Int, [CompactBlock])
defragStep (count, bs) = go count bs
    where
        go ::  Int -> [CompactBlock] -> (Int, [CompactBlock])
        go count bs
            | count >= length files = (count, bs)
            -- The only empty index is to the right of the file
            -- if none of the empties are large enough, we are done
            -- return it without the file
            | null largeEnoughEmpties = (count + 1, bs)
            | emptyIdx >= fileIdx = (count + 1, bs)
            -- Otherwise, we swap the first empty with the file
            -- making sure to add an empty if there is left over
            | otherwise = (count, mergeEmpty $ sublist 0 emptyIdx bs ++
                                           [file] ++
                                           leftOverEmpty ++
                                           sublist (emptyIdx + 1) (fileIdx) bs ++
                                           [CompactFree $ size file] ++
                                           sublist (fileIdx + 1) (length bs) bs)
        files = findIndices isCompactFile bs
        fileIdx = reverse files !! max 0 count
        file = bs !! fileIdx
        emptiesIdx = findIndices isCompactEmpty bs
        largeEnoughEmpties = filter (\i -> size (bs !! i) >= size file) emptiesIdx
        emptyIdx = head largeEnoughEmpties
        leftOverSize = if null largeEnoughEmpties then 0 else size (bs !! head largeEnoughEmpties) - size file
        leftOverEmpty = [CompactFree leftOverSize | leftOverSize > 0]

-- write a function to merge the CompactEmpty blocks
-- into a single block
mergeEmpty :: [CompactBlock] -> [CompactBlock]
mergeEmpty [] = []
mergeEmpty (CompactFree s : CompactFree s' : xs) =  CompactFree (s + s') : mergeEmpty xs
mergeEmpty (x : xs) = x : mergeEmpty xs

sublist :: Int -> Int -> [a] -> [a]
sublist start end xs = take (end - start) (drop start xs)

compact :: [Block] -> [CompactBlock]
compact = go
  where
    go [] = []
    go (f@(File i) : xs) = CompactFile i (1 + length fs) : go rest
      where
        (fs, rest) = span (== f) xs
    go (Free : xs) = CompactFree (1 + length fs) : go rest
      where
        (fs, rest) = span isFree xs

takeUntilConverges :: (Eq a) => (a -> a) -> a -> [a]
takeUntilConverges f = takeUntil
  where
    takeUntil current
      | next == current = [current]
      | otherwise = current : takeUntil next
      where
        next = f current

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
      -- Part 2
      print "Starting part 2"
      let c = compact dm
      let numFiles =  length $ filter isCompactFile c
      let steps = takeWhile (\(i, _) -> i < numFiles) $ iterate defragStep (0, c)
      let solution = snd $ last steps
      -- printDiskMap $ unpack solution
      --- printCompactDiskMap $ snd $ last steps
      let answer2 = sum $ zipWith (*) [0::Int ..] (ids . unpack $ solution)
      print answer2
