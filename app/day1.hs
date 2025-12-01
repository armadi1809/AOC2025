module Main where

import Data.Foldable (Foldable (fold))
import Data.List.Split (splitOn)

parseRotation :: String -> (Char, Int, Int)
parseRotation (x : xs) =
  let rot = read xs :: Int
   in (x, rot `mod` 100, rot `div` 100)

normalizeScore :: Int -> Int
normalizeScore score
  | score > 99 = score `mod` 100
  | score < 0 = score + 100
  | otherwise = score

calcNewScore :: Char -> Int -> Int -> Int
calcNewScore dir score rotMod =
  let newScore = if dir == 'L' then score - rotMod else score + rotMod
   in normalizeScore newScore

calcNewScoreWithNumOfZeroes :: String -> Int -> (Int, Int)
calcNewScoreWithNumOfZeroes action score =
  let (dir, rotMod, rotDiv) = parseRotation action
      newScore = calcNewScore dir score rotMod
      finalNumZeros
        | score /= 0 && newScore > score && dir == 'L' = rotDiv + 1
        | score /= 0 && newScore < score && dir == 'R' = rotDiv + 1
        | newScore == 0 = rotDiv + 1
        | otherwise = rotDiv
   in (newScore, finalNumZeros)

step1 :: (Int, Int) -> String -> (Int, Int)
step1 (score, numberOfZeroes) directive =
  let (dir, rotMod, _) = parseRotation directive
      newScore = calcNewScore dir score rotMod
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
