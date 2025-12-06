module Main where

import Data.List (sortOn)
import Data.List.Split (splitOn)

getRangeFromString :: String -> (Int, Int)
getRangeFromString range =
  let rangeExtremes = splitOn "-" range
   in (read (head rangeExtremes), read (rangeExtremes !! 1))

isValidId :: [String] -> String -> Int
isValidId ranges idstr =
  let idnum = read idstr
      filteredRanges =
        filter
          ( \range ->
              let (start, end) = getRangeFromString range
               in idnum >= start && idnum <= end
          )
          ranges
   in if null filteredRanges then 0 else 1

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges (r : rs) = foldl merge [r] rs
  where
    merge acc@(x@(s1, e1) : xs) (s2, e2)
      | s2 <= e1 + 1 = (s1, max e1 e2) : xs
      | otherwise = (s2, e2) : acc

countIDs :: [(Int, Int)] -> Int
countIDs ranges = sum [e - s + 1 | (s, e) <- ranges]

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day5.txt"
  let rangesAndIds = splitOn "\n\n" inp
  let ranges = splitOn "\n" (head rangesAndIds)
  let ids = splitOn "\n" (rangesAndIds !! 1)
  let res1 = sum (map (isValidId ranges) ids)
  print res1

  let rangeNums = sortOn fst (map getRangeFromString ranges)
  let merged = reverse (mergeRanges rangeNums)
  let res2 = countIDs merged
  print res2
