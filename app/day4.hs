module Main where

import Data.List.Split (splitOn)

main :: IO ()
main = do
  inp <- readFile "./inputs/day/4"
  let inpGrid = splitOn "\n" inp
  print inpGrid