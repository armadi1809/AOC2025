module Main where

import Data.List (elemIndex, nub)
import Data.List.Split (splitOn)

step :: ([Int], Int) -> String -> ([Int], Int)
step (beams, splits) row =
  let newBeams = map (\beam -> if row !! beam == '^' then (beam + 1, beam - 1) else (beam, beam)) beams
      additionalSplits = sum (map (\beam -> if (row !! beam) == '^' then 1 else 0) beams)
      filteredBeams = nub (concatMap (\(x, y) -> [x, y]) newBeams)
      finalBeams = filter (\b -> b >= 0 && b < length row) filteredBeams
   in (finalBeams, splits + additionalSplits)

calculateTotalSplits :: [String] -> Int
calculateTotalSplits grid =
  let sPos = case elemIndex 'S' (head grid) of
        Just x -> x
        Nothing -> error "impossible"
      res = foldl step ([sPos], 0) (tail grid)
   in snd res

step2 :: [Int] -> String -> [Int]
step2 counts row =
  let w = length row
      next0 = replicate w 0
      addAt xs i v =
        if i >= 0 && i < w
          then [if j == i then x + v else x | (x, j) <- zip xs [0 ..]]
          else xs
      propagate acc i =
        let c = counts !! i
         in if c == 0
              then acc
              else
                if row !! i == '^'
                  then addAt (addAt acc (i - 1) c) (i + 1) c
                  else addAt acc i c
   in foldl propagate next0 [0 .. w - 1]

calculateTotalTimelines :: [String] -> Int
calculateTotalTimelines grid =
  let firstRow = head grid
      sPos = case elemIndex 'S' firstRow of
        Just x -> x
        Nothing -> error "No S in first row"
      w = length firstRow
      initial = [if i == sPos then 1 else 0 | i <- [0 .. w - 1]]
      finalCounts = foldl step2 initial (tail grid)
   in sum finalCounts

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day7.txt"
  let inpGrid = splitOn "\n" inp

  let res1 = calculateTotalSplits inpGrid
  print res1

  let res2 = calculateTotalTimelines inpGrid
  print res2
