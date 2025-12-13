module Main where

import Data.List.Split (splitOn)

type Present = [[Bool]]

type GridInfo = ((Int, Int), [Int])

processPresent :: String -> Present
processPresent presentStr =
  let presShapeStr = concat (drop 1 (splitOn ":\n" presentStr))
   in map (map (== '#')) (splitOn "\n" presShapeStr)

processGrid :: String -> GridInfo
processGrid gridStr =
  let shapeAndPresents = splitOn ": " gridStr
      shape = map read (take 2 (splitOn "x" (head shapeAndPresents)))
      presentsMap = map read (splitOn " " (shapeAndPresents !! 1))
   in ((head shape, shape !! 1), presentsMap)

countTrues :: [Bool] -> Int
countTrues = length . filter id

countTruesInPresent :: Present -> Int
countTruesInPresent present = countTrues (concat present)

isRegionValid :: [Present] -> GridInfo -> Bool
isRegionValid presents ((h, w), presentCounts) =
  let numTruesInPresents =
        sum
          ( map
              ( \i ->
                  let presCount = presentCounts !! i
                   in if presCount == 0 then 0 else presCount * countTruesInPresent (presents !! i)
              )
              [0 .. 5]
          )
   in numTruesInPresents <= h * w

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day12.txt"
  let gridAndPresents = splitOn "\n\n" inp
  let presentsStrs = take 6 gridAndPresents
  let gridStrs = drop 6 gridAndPresents
  let presents = map processPresent presentsStrs
  let grids = map processGrid (splitOn "\n" (head gridStrs))
  let res1 = length $ filter id (map (isRegionValid presents) grids)
  print res1