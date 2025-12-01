module Main where

import Data.Foldable (Foldable (fold))
import Data.List.Split (splitOn)

calcNewScore :: String -> Int -> Int
calcNewScore (x : xs) score =
  let rot = (read xs :: Int) `mod` 100
      newScore = if x == 'L' then score - rot else score + rot
   in if newScore > 99
        then newScore `mod` 100
        else
          if newScore < 0
            then newScore + 100
            else newScore

calcNewScoreWithNumOfZeroes :: String -> Int -> (Int, Int)
calcNewScoreWithNumOfZeroes (x : xs) score =
  let rot = (read xs :: Int)
      numZeros = rot `div` 100
      newScore = calcNewScore (x : xs) score
      finalNumZeros
        | score /= 0 && newScore > score && x == 'L' = numZeros + 1
        | score /= 0 && newScore < score && x == 'R' = numZeros + 1
        | newScore == 0 = numZeros + 1
        | otherwise = numZeros
   in (newScore, finalNumZeros)

step1 :: (Int, Int) -> String -> (Int, Int)
step1 (score, numberOfZeroes) directive =
  let newScore = calcNewScore directive score
   in if newScore == 0 then (newScore, numberOfZeroes + 1) else (newScore, numberOfZeroes)

step2 :: (Int, Int) -> String -> (Int, Int)
step2 (score, numberOfZeroes) directive =
  let (newScore, numZeroes) = calcNewScoreWithNumOfZeroes directive score
   in (newScore, numberOfZeroes + numZeroes)

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day1.txt"
  let inpList = splitOn "\n" inp

  let resPart1 = foldl step1 (50, 0) inpList
  print "Part 1"
  print resPart1

  let resPart2 = foldl step2 (50, 0) inpList
  print "Part 2"
  print resPart2
