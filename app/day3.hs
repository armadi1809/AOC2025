module Main where

import Data.Char (digitToInt)
import Data.Foldable (Foldable (fold), foldl')
import Data.List (elemIndex)
import Data.List.Split (chunksOf, splitOn)
import GHC.Num.Integer (integerToNaturalClamp)

largestKSubseq :: Int -> String -> String
largestKSubseq = go
  where
    go 0 _ = []
    go need xs
      | length xs == need = xs
      | otherwise =
          let windowSize = length xs - need + 1
              window = take windowSize xs
              m = maximum window
              Just idx = elemIndex m window
           in m : go (need - 1) (drop (idx + 1) xs)

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day3.txt"
  let inpList = splitOn "\n" inp
  let res1 = sum (map (read . largestKSubseq 2) inpList)
  let res2 = sum (map (read . largestKSubseq 12) inpList)
  print res1
  print res2