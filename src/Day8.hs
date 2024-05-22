module Day8 where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)

type StringMap = Map.Map String (String, String)

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

lookUp :: StringMap -> Char -> String -> String
lookUp map lr v
  | lr == 'L' = fst $ fromMaybe ("", "") val
  | lr == 'R' = snd $ fromMaybe ("", "") val
  where
    val = Map.lookup v map

countSteps :: String -> String -> String -> StringMap -> Int -> Int
countSteps instructions "" v map count = countSteps instructions instructions v map count
countSteps instructions (x:xs) v map count
  | val == "ZZZ" = count + 1
  | otherwise = countSteps instructions xs val map (count + 1)
  where
    val = lookUp map x v

getMap :: [(String, (String, String))] -> [String] -> StringMap
getMap kv [] = Map.fromList kv
getMap kv [ "" ] = Map.fromList kv
getMap kv (x:xs) = let (k, v) = splitOnFirst '=' $ filter (\a -> not (isSpace a || a == ')' || a == '(' )) x
                    in getMap (kv++[(k, splitOnFirst ',' v)]) xs

solve1 :: [String] -> Int
solve1 str = do
  let map = getMap [] $ tail $ tail str
  let instructions = head str
  countSteps  instructions instructions "AAA" map 0

countSteps2 :: String -> String -> StringMap -> Int -> String -> Int
countSteps2 instructions "" map count v = countSteps2 instructions instructions map count v
countSteps2 instructions (x:xs) map count v
  | last val == 'Z' = count + 1
  | otherwise = countSteps2 instructions xs map (count + 1) val
  where
    val = lookUp map x v

getStarts :: [String] -> [String]
getStarts [] = []
getStarts (x:xs)
  | last key == 'A' = key:getStarts xs
  | otherwise = getStarts xs
  where
    key = filter (not . isSpace) $ fst $ splitOnFirst '=' x

lcmRec :: [Int] -> Int
lcmRec [x, xs] = lcm x xs
lcmRec (x:xs:xss) = lcm (lcm x xs) (lcmRec xss)

solve2 :: [String] -> Int
solve2 str = do
  let mapping = getMap [] $ tail $ tail str
  let instructions = head str
  let starts = getStarts $ tail $ tail str
  lcmRec $ map (countSteps2 instructions instructions mapping 0) starts
