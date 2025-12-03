module Main where

import Data.Char (digitToInt)
import Data.Foldable (Foldable (fold), foldl')
import Data.List (elemIndex)
import Data.List.Split (chunksOf, splitOn)
import GHC.Num.Integer (integerToNaturalClamp)

maxJoltageFromStr :: (Int, Int) -> String -> Int
maxJoltageFromStr (leftHighest, rightHighest) [] = leftHighest * 10 + rightHighest
maxJoltageFromStr (leftHighest, rightHighest) (d : rest) =
  if null rest
    then
      let dig = digitToInt d
       in if dig > rightHighest
            then maxJoltageFromStr (leftHighest, dig) []
            else maxJoltageFromStr (leftHighest, rightHighest) []
    else
      let dig = digitToInt d
       in if dig > leftHighest
            then maxJoltageFromStr (dig, 0) rest
            else
              if dig > rightHighest
                then maxJoltageFromStr (leftHighest, dig) rest
                else maxJoltageFromStr (leftHighest, rightHighest) rest

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
  let res1 = sum (map (maxJoltageFromStr (0, 0)) inpList)
  let res2 = sum (map (read . largestKSubseq 12) inpList)
  print res1
  print res2