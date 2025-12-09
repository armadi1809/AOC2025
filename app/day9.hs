module Main where

import Data.List.Split (splitOn)

type Point = (Int, Int)

type Edge = (Int, Point, Point)

rectArea :: Point -> Point -> Int
rectArea (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

parsePoint :: String -> Point
parsePoint s =
  let [a, b] = map read (splitOn "," s)
   in (a, b)

allEdges :: [Point] -> [Edge]
allEdges pts =
  [ (rectArea a b, min a b, max a b)
    | (a, i) <- zip pts [0 ..],
      (b, j) <- zip pts [0 ..],
      i < j
  ]

getMaxRectArea :: [String] -> Int
getMaxRectArea pts =
  let points = map parsePoint pts
      edges = allEdges points
      (area, _, _) = maximum edges
   in area

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day9.txt"
  let lns = filter (not . null) (splitOn "\n" inp)
  let res1 = getMaxRectArea lns
  print res1
