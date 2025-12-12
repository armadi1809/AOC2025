module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as Map

type GraphMap = Map.Map String [String]

processLine :: String -> (String, [String])
processLine line =
  let s = splitOn ": " line
      parent = head s
      children = splitOn " " (s !! 1)
   in (parent, children)

processLines :: [String] -> [(String, [String])]
processLines = map processLine

mapFromLines :: [String] -> GraphMap
mapFromLines lines =
  let graphArray = processLines lines
      m = Map.fromList graphArray
   in m

dfs :: GraphMap -> Int -> String -> Int
dfs graph currCount start =
  let neighborsM = Map.lookup start graph
   in case neighborsM of
        Nothing -> currCount
        Just neighbors ->
          if head neighbors == "out"
            then currCount + 1
            else
              let neighborsCounts = map (dfs graph 0) neighbors
               in currCount + sum neighborsCounts

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day11.txt"
  let lines = splitOn "\n" inp
  let graph = mapFromLines lines
  let res1 = dfs graph 0 "you"
  print res1