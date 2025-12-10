module Main where

import Data.List.Split (splitOn)

type Point = (Int, Int)

parsePoint :: String -> Point
parsePoint s =
  let [a, b] = map read (splitOn "," s)
   in (a, b)

rectArea :: Point -> Point -> Int
rectArea (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

-- Check if point is inside polygon using ray casting
isInsideOrOn :: [Point] -> Point -> Bool
isInsideOrOn polygon (px, py) =
  let edges = zip polygon (tail polygon ++ [head polygon])
      -- First check if on any edge
      onEdge ((x1, y1), (x2, y2))
        | x1 == x2 && px == x1 = py >= min y1 y2 && py <= max y1 y2
        | y1 == y2 && py == y1 = px >= min x1 x2 && px <= max x1 x2
        | otherwise = False
   in any onEdge edges || odd (length $ filter (crossesRay (px, py)) edges)
  where
    crossesRay (px, py) ((x1, y1), (x2, y2))
      | y1 == y2 = False -- horizontal edge
      | x1 /= x2 = False -- not vertical
      | x1 <= px = False -- edge is to the left or at point
      | otherwise = py > min y1 y2 && py <= max y1 y2

-- Check if a polygon edge crosses into the interior of a rectangle
edgeCrossesRect :: Point -> Point -> (Point, Point) -> Bool
edgeCrossesRect (rx1, ry1) (rx2, ry2) ((ex1, ey1), (ex2, ey2)) =
  let minRx = min rx1 rx2
      maxRx = max rx1 rx2
      minRy = min ry1 ry2
      maxRy = max ry1 ry2
      minEx = min ex1 ex2
      maxEx = max ex1 ex2
      minEy = min ey1 ey2
      maxEy = max ey1 ey2
   in if ex1 == ex2
        -- Vertical edge: crosses if x is strictly inside rect and y range overlaps interior
        then ex1 > minRx && ex1 < maxRx && maxEy > minRy && minEy < maxRy
        -- Horizontal edge: crosses if y is strictly inside rect and x range overlaps interior
        else ey1 > minRy && ey1 < maxRy && maxEx > minRx && minEx < maxRx

-- Check if rectangle is valid: all 4 corners inside/on polygon, no edge crosses interior
isValidRect :: [Point] -> Point -> Point -> Bool
isValidRect polygon p1@(x1, y1) p2@(x2, y2) =
  let corners = [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]
      edges = zip polygon (tail polygon ++ [head polygon])
      allCornersInside = all (isInsideOrOn polygon) corners
      noEdgeCrosses = not $ any (edgeCrossesRect p1 p2) edges
   in allCornersInside && noEdgeCrosses

-- Part 1
getMaxRectArea :: [Point] -> Int
getMaxRectArea points =
  let pairs = [(a, b) | (a, i) <- zip points [0 ..], (b, j) <- zip points [0 ..], i < j]
   in maximum (map (uncurry rectArea) pairs)

-- Part 2
getMaxValidRectArea :: [Point] -> Int
getMaxValidRectArea points =
  let pairs = [(a, b) | (a, i) <- zip points [0 ..], (b, j) <- zip points [0 ..], i < j]
      validPairs = filter (uncurry (isValidRect points)) pairs
   in if null validPairs then 0 else maximum (map (uncurry rectArea) validPairs)

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day9.txt"
  let lns = filter (not . null) (splitOn "\n" inp)
  let points = map parsePoint lns
  print $ getMaxRectArea points
  print $ getMaxValidRectArea points