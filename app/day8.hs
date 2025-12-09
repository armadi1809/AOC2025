module Main where

import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

type Point = (Int, Int, Int)

type Edge = (Double, Point, Point)

dist :: Point -> Point -> Double
dist (x1, y1, z1) (x2, y2, z2) =
  sqrt . fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

parsePoint :: String -> Point
parsePoint s =
  let [a, b, c] = map read (splitOn "," s)
   in (a, b, c)

findGroup :: Point -> [[Point]] -> Maybe ([Point], [[Point]])
findGroup _ [] = Nothing
findGroup p (g : gs)
  | p `elem` g = Just (g, gs)
  | otherwise = case findGroup p gs of
      Nothing -> Nothing
      Just (found, rest) -> Just (found, g : rest)

-- Returns (didMerge, newGroups)
mergePoints :: Point -> Point -> [[Point]] -> (Bool, [[Point]])
mergePoints a b groups =
  case findGroup a groups of
    Nothing -> case findGroup b groups of
      Nothing -> (True, [a, b] : groups)
      Just (gb, rest) -> (True, (a : gb) : rest)
    Just (ga, restA) ->
      if b `elem` ga
        then (False, groups)
        else case findGroup b restA of
          Nothing -> (True, (b : ga) : restA)
          Just (gb, restB) -> (True, (ga ++ gb) : restB)

allEdges :: [Point] -> [Edge]
allEdges pts =
  [ (dist a b, min a b, max a b)
    | (a, i) <- zip pts [0 ..],
      (b, j) <- zip pts [0 ..],
      i < j
  ]

sortEdges :: [Edge] -> [Edge]
sortEdges = sortBy (comparing (\(d, p1, p2) -> (d, p1, p2)))

-- Part 1: Process n edges
processEdges :: Int -> [[Point]] -> [Edge] -> [[Point]]
processEdges 0 groups _ = groups
processEdges _ groups [] = groups
processEdges n groups ((_, a, b) : es) =
  let (_, groups') = mergePoints a b groups
   in processEdges (n - 1) groups' es

-- Part 2: Find the edge that connects everything into one group
-- Returns the last edge that caused a merge when we reach 1 group
findLastMerge :: Int -> [[Point]] -> [Edge] -> (Point, Point)
findLastMerge totalPoints groups [] = error "Ran out of edges"
findLastMerge totalPoints groups ((_, a, b) : es) =
  let (didMerge, groups') = mergePoints a b groups
   in if didMerge && length (concat groups') == totalPoints && length groups' == 1
        then (a, b) -- This edge connected everything!
        else findLastMerge totalPoints groups' es

simulate :: Int -> [Point] -> Int
simulate n points =
  let edges = sortEdges (allEdges points)
      groupsAfter = processEdges n [] edges
      touchedPoints = concat groupsAfter
      orphans = filter (`notElem` touchedPoints) points
      allGroups = groupsAfter ++ map (: []) orphans
      sizes = reverse $ sort (map length allGroups)
   in product (take 3 sizes)

simulate2 :: [Point] -> Int
simulate2 points =
  let edges = sortEdges (allEdges points)
      ((x1, _, _), (x2, _, _)) = findLastMerge (length points) [] edges
   in x1 * x2

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day8.txt"
  let lns = filter (not . null) (splitOn "\n" inp)
  let points = map parsePoint lns
  let res1 = simulate 1000 points
  print res1
  let res2 = simulate2 points
  print res2