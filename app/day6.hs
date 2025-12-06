module Main where

import Data.Char (isSpace)
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

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day6.txt"
  let inpGrid = splitOn "\n" inp
  let res1 = solveProblems inpGrid
  print res1
