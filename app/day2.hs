module Main where

import Data.Foldable (Foldable (fold))
import Data.List.Split (chunksOf, splitOn)

hasRepeatingSequence :: Int -> Bool
hasRepeatingSequence n =
  let s = show n
      len = length s
      possibleLens = [1 .. len `div` 2]
   in any
        ( \l ->
            let (first, rest) = splitAt l s
             in rest /= "" && all (== first) (chunksOf l s)
        )
        possibleLens

addId1 :: Int -> Int -> Int
addId1 acc newId =
  let newIdStr = show newId
      n = length newIdStr
      isInValid = even n && take (n `div` 2) newIdStr == drop (n `div` 2) newIdStr
   in if isInValid then acc + newId else acc

addId2 :: Int -> Int -> Int
addId2 acc newId =
  if hasRepeatingSequence newId then acc + newId else acc

getInvalidIdsSumFromRange :: String -> Int -> Int -> Int
getInvalidIdsSumFromRange version start end =
  let add = if version == "1" then addId1 else addId2
   in foldl add 0 [start .. end]

getInvalidIdsSumFromStrRange :: String -> String -> Int
getInvalidIdsSumFromStrRange version range =
  let rangeArr = splitOn "-" range
      start = read (head rangeArr)
      end = read (rangeArr !! 1)
   in getInvalidIdsSumFromRange version start end

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day2.txt"
  let inpList = splitOn "," inp
      res1 = sum (map (getInvalidIdsSumFromStrRange "1") inpList)
  print res1

  let res2 = sum (map (getInvalidIdsSumFromStrRange "2") inpList)
  print res2