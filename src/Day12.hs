module Day12 where

import Data.List (groupBy, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type Arrangements = Map.Map (String, Int, [Int]) Int

arrangements :: Arrangements -> Int -> (String, [Int]) -> (Arrangements, Int)
arrangements arr 0 ([], []) = (arr, 1)
arrangements arr _ ([], _) = (arr, 0)
arrangements arr count ('.': xs, groups)
  | Just counts <- cache = (arr, counts)
  | count == 0 = do
    let (arr1, counts) = arrangements arr 0 (xs, groups)
    (Map.insert ('.':xs, 0, groups) counts arr1, counts)
  | null groups && count /= 0 = (arr, 0)
  | null groups = do
    let (arr1, counts) = arrangements arr count (xs, groups)
    (Map.insert ('.':xs, count, groups) counts arr1, counts)
  | head groups == count = do
    let (arr1, counts) = arrangements arr 0 (xs, tail groups)
    (Map.insert ('.':xs, count, groups) counts arr1, counts)
  | otherwise = (arr, 0)
  where
    cache = Map.lookup ('.':xs, count, groups) arr
arrangements arr count ('?': xs, groups)
  | Just counts <- cache = (arr, counts)
  | otherwise = do
    let (arr1, counts1) = arrangements arr count ('#':xs, groups)
    let (arr2, counts2) = arrangements arr1 count ('.':xs, groups)
    (Map.insert ('?':xs, count, groups) (counts1 + counts2) arr2, counts1 + counts2)
  where
    cache = Map.lookup ('?':xs, count, groups) arr
arrangements arr count ('#': xs, groups)
  | Just counts <- cache = (arr, counts)
  | otherwise = do
    let (arr1, counts) = arrangements arr (count + 1) (xs, groups)
    (Map.insert ('#':xs, count, groups) counts arr1, counts)
  where
    cache = Map.lookup ('#':xs, count, groups) arr

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

parse :: [String] -> [(String, [Int])]
parse [] = []
parse (x:xs) = let (grid, i) = splitOnFirst ' ' x
  in (grid, map read $ splitOn "," i) : parse xs

solve1 :: [String] -> Int
solve1 str = sum $ map ((snd . arrangements Map.empty 0) . (\(x, y) -> (x ++ ".", y))) (parse str)

solve2 :: [String] -> Int
solve2 str = do
  let input = map (\(x, y) -> (intercalate "?" (replicate 5 x) ++ ".", concat $ replicate 5 y)) $ parse str
  sum $ map (snd . arrangements Map.empty 0) input

