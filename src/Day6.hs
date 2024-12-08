module Main where

import Data.List (nub)
import Data.Set qualified as Set
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Control.DeepSeq (NFData,rnf)

-- Define a type alias for simplicity
type Parser = Parsec Void String

data Tile = Obstruction | Empty Char
  deriving (Eq)

instance Show Tile where
  show Obstruction = "#"
  show (Empty c) = [c]

instance NFData Tile where
  rnf Obstruction = ()
  rnf (Empty c) = rnf c

isEmptyTile :: Maybe Tile -> Bool
isEmptyTile (Just (Empty _)) = True
isEmptyTile _ = False

-- Parser for a single integer
tile :: Parser Tile
tile =
  choice
    [ Obstruction <$ char '#', -- Use <$ to return a fixed value
      Empty <$> (char '.' <|> char '^') -- Use <$> to pass the matched char into Empty
    ]

row :: Parser [Tile]
row = many tile

puzzle :: Parser [[Tile]]
puzzle = row `sepBy` newline

startTile :: [[Tile]] -> Maybe (Int, Int)
startTile ts = search 0 0 ts
  where
    rows = length ts
    cols = if null ts then 0 else length (head ts)
    search x y ts
      | y >= rows = Nothing
      | x >= cols = search 0 (y + 1) ts
      | ts !! y !! x == Empty '^' = Just (x, y)
      | otherwise = search (x + 1) y ts

data Direction = North | East | South | West
  deriving (Eq, Show, Ord)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

step :: Direction -> (Int, Int) -> (Int, Int)
step North (x, y) = (x, y - 1)
step East (x, y) = (x + 1, y)
step South (x, y) = (x, y + 1)
step West (x, y) = (x - 1, y)

tileAt :: [[Tile]] -> (Int, Int) -> Maybe Tile
tileAt ts (x, y)
  | x < 0 || y < 0 || x >= cols || y >= rows = Nothing
  | otherwise = Just $ ts !! y !! x
  where
    rows = length ts
    cols = if null ts then 0 else length (head ts)

stepGuard :: [[Tile]] -> (Direction, (Int, Int)) -> (Direction, (Int, Int))
stepGuard ts (d, (x, y))
  -- if we are at the end we are done
  | x < 0 || y < 0 || x >= cols || y >= rows = (d, (x, y))
  | nextTile == Just Obstruction = (rightDirection, nextPosRight)
  | otherwise = (d, nextPos)
  where
    rows = length ts
    cols = if null ts then 0 else length (head ts)
    rightDirection = turnRight d
    nextPosRight = step rightDirection (x, y)
    nextPos = step d (x, y)
    nextTile = tileAt ts (step d (x, y))

takeUntilConverges :: (Eq a) => (a -> a) -> a -> [a]
takeUntilConverges f = takeUntil
  where
    takeUntil current
      | next == current = [current]
      | otherwise = current : takeUntil next
      where
        next = f current

-- Detects if the guard's path forms a cycle
detectCycle :: [[Tile]] -> (Direction, (Int, Int)) -> Bool
detectCycle p = go Set.empty
  where
    go visited state@(d, pos)
      | state `Set.member` visited = True -- Cycle detected
      | outOfBounds pos = False -- Guard left the map
      | otherwise = go (Set.insert state visited) (stepGuard p state)

    -- Check if a position is out of bounds
    outOfBounds (x, y) = x < 0 || y < 0 || x >= cols || y >= rows
    -- Map dimensions
    rows = length p
    cols = if null p then 0 else length (head p)

replace :: [Tile] -> Int -> Tile -> [Tile]
replace (_ : xs) 0 t = t : xs
replace (x : xs) i t = x : replace xs (i - 1) t
replace [] _ _ = []

replaceTile :: [[Tile]] -> (Int, Int) -> Tile -> [[Tile]]
replaceTile (a : as) (x, 0) t = replace a x t : as
replaceTile (a : as) (x, y) t = a : replaceTile as (x, y - 1) t
replaceTile [] _ _ = []

printPuzzle :: [[Tile]] -> IO ()
printPuzzle [] = return ()
printPuzzle (x : xs) = do
  putStrLn $ foldl (\acc t -> acc ++ show t) "" x
  printPuzzle xs

main :: IO ()
main = do
  input <- readFile "input/Day6.txt"
  case parse puzzle "Day5.txt" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right p -> do
      case startTile p of
        Nothing -> putStrLn "No start tile found!"
        Just startPos -> do
          let steps = takeUntilConverges (stepGuard p) (North, startPos)
          -- Remove the last step since it's outside the puzzle
          let withoutLastStep = init steps
          let tiles = map snd withoutLastStep
          print $ length $ nub tiles
          -- My idea: take every step and calculate the next step
          -- but pretend there is a obstruction
          -- if the next step would result in a value in the original
          -- list we have cycled
          let rows = length p
          let cols = if null p then 0 else length (head p)
          let allPotentialObstacles =
                [ (x, y)
                | y <- [0 .. rows - 1],
                    x <- [0 .. cols - 1],
                    tileAt p (x, y) == Just (Empty '.') -- Ensure the tile is empty
                ]
          
          let potentialObstacles =
                map snd $
                  filter
                    ( \(d, pos) ->
                        let nextTile = tileAt p (step d pos)
                         in isEmptyTile nextTile
                    )
                    (drop 1 withoutLastStep)
          let potentialPuzzles =
                map
                  ( \pos ->
                      (pos, replaceTile p pos Obstruction)
                  )
                  allPotentialObstacles
          let obstructionCycles = (filter (\(_, p') -> detectCycle p' (North, startPos)) potentialPuzzles) `using` parList rdeepseq
          -- print each solution
        --   mapM_ (\cycle -> do
        --         printPuzzle (snd cycle)
        --         putStrLn ""
        --     ) obstructionCycles
          print $ length obstructionCycles
