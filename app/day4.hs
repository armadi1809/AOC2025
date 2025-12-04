module Main where

import Data.List (foldl')
import Data.List.Split (splitOn)

isValid :: [Char] -> Char -> Bool
isValid neighbors cell =
  let rolls = filter (== '@') neighbors
   in length rolls < 4 && cell == '@'

update2D :: Int -> Int -> a -> [[a]] -> [[a]]
update2D row col val xs =
  [ if i == row
      then updateAt col val rowVals
      else rowVals
    | (i, rowVals) <- zip [0 ..] xs
  ]
  where
    updateAt j v ys = [if k == j then v else y | (y, k) <- zip ys [0 ..]]

countValidRolls :: [[Char]] -> (Int, [(Int, Int)])
countValidRolls grid =
  let h = length grid
      w = length (head grid)
      valids =
        map
          ( \i ->
              map
                ( \j ->
                    let neighborsInds = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1), (i + 1, j + 1), (i - 1, j - 1), (i + 1, j - 1), (i - 1, j + 1)]
                        neighborsIndsFiltered = filter (\(ni, nj) -> ni >= 0 && ni < h && nj >= 0 && nj < w) neighborsInds
                        neighbors = map (\(ni, nj) -> (grid !! ni) !! nj) neighborsIndsFiltered
                     in (isValid neighbors ((grid !! i) !! j), (i, j))
                )
                [0 .. w - 1]
          )
          [0 .. h - 1]
      countValid = sum (map (\(valid, _) -> if valid then (1 :: Int) else (0 :: Int)) (concat valids))
      validInds = map snd (concatMap (filter fst) valids)
   in (countValid, validInds)

countNumberUpdates :: [[Char]] -> Int -> Int
countNumberUpdates grid curr =
  let (countValid, validInds) = countValidRolls grid
   in if countValid == 0
        then curr
        else
          let newGrid = foldl' (\acc (r, c) -> update2D r c '.' acc) grid validInds
           in countNumberUpdates newGrid (curr + countValid)

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day4.txt"
  let inpGrid = splitOn "\n" inp
  let res1 = fst $ countValidRolls inpGrid
  print res1
  let res2 = countNumberUpdates inpGrid 0
  print res2