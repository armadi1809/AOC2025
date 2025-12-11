module Main where

import Data.Bits
import Data.List.Split (splitOn)

type State = [Bool] -- e.g. [False, True, True, False]

type Button = [Bool]

toggle :: State -> Button -> State
toggle = zipWith (/=)

toBoolMask :: String -> State
toBoolMask = map (/= '.')

parsePattern :: String -> State
parsePattern pattern_inp =
  let patstring = (init . tail) pattern_inp
   in toBoolMask patstring

parseButton :: Int -> String -> Button
parseButton patternLength buttonsStr =
  let buttons = map read (splitOn "," ((init . tail) buttonsStr))
   in map (`elem` buttons) [0 .. patternLength - 1]

parseButtons :: Int -> [String] -> [Button]
parseButtons patternLength = map (parseButton patternLength)

bfs :: [Button] -> State -> State -> Int
bfs buttons start goal = search [(start, 0)] [start]
  where
    search [] _ = -1 -- no solution found
    search ((s, d) : rest) visited
      | s == goal = d
      | otherwise =
          let newStates = [toggle s b | b <- buttons]
              fresh = [ns | ns <- newStates, not (ns `elem` visited)]
              queue' = rest ++ [(ns, d + 1) | ns <- fresh]
              visited' = visited ++ fresh
           in search queue' visited'

processLine :: [String] -> Int
processLine inputs =
  let pattern = parsePattern (head inputs)
      buttons = parseButtons (length pattern) ((init . tail) inputs)
   in bfs buttons [False | _ <- [1 .. (length pattern)]] pattern

main :: IO ()
main = do
  inp <- readFile "./app/inputs/day10.txt"
  let lines = splitOn "\n" inp
  let line_items = map (splitOn " ") lines
  let res1 = sum (map processLine line_items)
  print res1