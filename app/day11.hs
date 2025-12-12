module Main where

import Data.Foldable
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type GraphMap = Map.Map String [String]

type Memo = Map.Map (String, Bool, Bool) Int

processLine :: String -> (String, [String])
processLine line =
  let s = splitOn ": " line
      parent = head s
      children = splitOn " " (s !! 1)
   in (parent, children)

mapFromLines :: [String] -> GraphMap
mapFromLines = Map.fromList . map processLine

dfs :: GraphMap -> String -> Int
dfs graph start =
  case Map.lookup start graph of
    Nothing -> 0
    Just neighbors ->
      if head neighbors == "out"
        then 1
        else sum $ map (dfs graph) neighbors

dfs2 :: GraphMap -> String -> Int
dfs2 graph start = fst $ go Map.empty False False start
  where
    go :: Memo -> Bool -> Bool -> String -> (Int, Memo)
    go memo dac fft node =
      let dac' = dac || node == "dac"
          fft' = fft || node == "fft"
          key = (node, dac', fft')
       in case Map.lookup key memo of
            Just cached -> (cached, memo) -- Already computed!
            Nothing ->
              case Map.lookup node graph of
                Nothing -> (0, memo)
                Just neighbors ->
                  if head neighbors == "out"
                    then
                      let r = if dac' && fft' then 1 else 0
                       in (r, Map.insert key r memo)
                    else
                      let (total, memo') = foldl' (go' dac' fft') (0, memo) neighbors
                       in (total, Map.insert key total memo')

    go' dac fft (acc, memo) neighbor =
      let (count, memo') = go memo dac fft neighbor
       in (acc + count, memo')

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day11.txt"
  let lines = splitOn "\n" inp
  let graph = mapFromLines lines
  let res1 = dfs graph "you"
  print res1
  let res2 = dfs2 graph "svr"
  print res2