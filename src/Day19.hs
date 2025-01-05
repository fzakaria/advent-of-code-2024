module Main where
import Text.Megaparsec ( Parsec, parse, errorBundlePretty, choice, sepBy, many )
import Data.Void (Void)
import Text.Megaparsec.Char (char, string, newline)
import Data.List (stripPrefix, isPrefixOf, intercalate)
import Data.Maybe (fromJust, catMaybes)
import Control.Applicative ((<|>), some)
import qualified Data.Set as Set
import Debug.Trace (trace)

type Parser = Parsec Void String

data Stripe = White | Blue | Black | Red | Green
    deriving (Eq, Ord)

instance Show Stripe where
    show :: Stripe -> String
    show White = "w"
    show Blue = "u"
    show Black = "b"
    show Red = "r"
    show Green = "g"

type Towel = [Stripe]
type Pattern = [Stripe]

data Puzzle = Puzzle [Towel] [Pattern]

showStripes :: [Stripe] -> String
showStripes = concatMap show

instance Show Puzzle where
    show :: Puzzle -> String
    show (Puzzle ts ps) = intercalate ", " (map showStripes ts) ++
                          "\n\n" ++ unlines (map showStripes ps)

stripe :: Parser Stripe
stripe = choice
    [ char 'w' >> return White,
      char 'u' >> return Blue,
      char 'b' >> return Black,
      char 'r' >> return Red,
      char 'g' >> return Green
    ]

towel :: Parser Towel
towel = many stripe

towels :: Parser [Towel]
towels = do
  towel `sepBy` string ", "

pattern :: Parser Pattern
pattern = many stripe

patterns :: Parser [Pattern]
patterns = pattern `sepBy` newline

puzzle :: Parser Puzzle
puzzle = do
  t <- towels
  _ <- some newline
  Puzzle t <$> patterns

same :: [Towel] -> Pattern -> Bool
same ts p = concat ts == p

combination :: [Towel] -> Pattern -> Maybe [Towel]
combination ts p = snd $ go Set.empty [] ts p
    where
        go :: Set.Set [Stripe] -> [Towel] -> [Towel] -> Pattern -> (Set.Set [Stripe], Maybe [Towel])
        go visited acc ts p
            | same acc p = (visited, Just acc)
            | null ts' = (visited, Nothing)
            | Set.member stripes visited = (visited, Nothing)
            | otherwise = foldl (\(v', res') t ->
                let (visited'', res) = go v' (acc ++ [t]) ts p
                in case res of
                    Just _ -> (visited'', res)
                    Nothing -> (Set.union visited'' visited, res')
                ) (visited', Nothing) ts'
            where
                stripes = concat acc
                visited' = Set.insert stripes visited
                -- remaining pattern to solve
                p' = fromJust $ stripPrefix stripes p
                -- possible towels that we can use
                ts':: [Towel] = filter (`isPrefixOf` p') ts

main :: IO ()
main = do
  input <- readFile "input/Day19.txt"
  case parse puzzle "Day19.txt" input of
    Prelude.Left err -> putStrLn $ errorBundlePretty err
    Prelude.Right p@(Puzzle ts ps) -> do
        print p
        let solutions = map (combination ts) ps
        print $ length (catMaybes solutions)