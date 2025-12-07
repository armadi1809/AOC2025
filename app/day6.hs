module Main where

import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.List.Split (splitOn)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

getResultFromString :: [String] -> String -> Int
getResultFromString numbers operator =
  let nums = map (\num -> read num) numbers
   in if operator == "*" then product nums else sum nums

solveProblems :: [String] -> Int
solveProblems grid =
  let h = length grid
      w = length (head grid)
      x = map (splitOn " ") grid
      xx = map (filter (not . null)) x
      operations = xx !! (h - 1)
      numbers = transpose (init xx)
      results = map (\ind -> getResultFromString (numbers !! ind) (operations !! ind)) [0 .. length operations - 1]
   in sum results

parseColumnToNumber :: String -> Maybe Int
parseColumnToNumber col =
  let digits = filter isDigit col
   in if null digits then Nothing else Just (read digits)

isSeparator :: String -> Bool
isSeparator = all isSpace

getOperator :: [String] -> Char
getOperator cols =
  let firstCol = head cols
      ops = filter (`elem` "*+") firstCol
   in if null ops then '+' else head ops

solveProblem :: [String] -> Int
solveProblem cols =
  let -- The operator is at the bottom of one of the columns
      op = getOperator cols
      -- Parse each column as a number, filter out Nothing values
      numbers = [n | Just n <- map parseColumnToNumber cols]
   in if op == '*' then product numbers else sum numbers

solveProblems' :: [String] -> Int
solveProblems' grid =
  let -- Transpose so each element is a column from the original grid
      transposed = transpose grid
      -- Group into problems: split by separator columns
      grouped = filter (not . null) $ splitOn' isSeparator transposed
      results = map solveProblem grouped
   in sum results

splitOn' :: (a -> Bool) -> [a] -> [[a]]
splitOn' _ [] = []
splitOn' p xs =
  let (chunk, rest) = break p xs
      rest' = dropWhile p rest
   in chunk : splitOn' p rest'

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day6.txt"
  let inpGrid = splitOn "\n" inp
  let res1 = solveProblems inpGrid
  print res1
  let res2 = solveProblems' inpGrid
  print res2
