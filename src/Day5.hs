module Day5 where

import Data.Char (isDigit)

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

mapOnce :: [(Int, Int, Int)] -> Int -> Int
mapOnce [] a = a
mapOnce ((x, y, z):xs) a
  | a < y = mapOnce xs a
  | a >= y + z = mapOnce xs a
  | otherwise = a - y + x

mapRec :: [[(Int, Int, Int)]] -> Int -> Int
mapRec xs seeds = foldl (flip mapOnce) seeds xs

minLocation :: [[(Int, Int, Int)]] -> [Int] -> Int
minLocation maps =  minimum . map (mapRec maps)

readNumbers :: String -> (Int, Int, Int)
readNumbers x = let [a, b, c] = map read $ words x :: [Int]
  in (a, b, c)

seeds :: String -> [Int]
seeds x = map read $ words $ snd $ splitOnFirst ':' x

mappings :: [String] -> [(Int, Int, Int)] -> [[(Int, Int, Int)]]
mappings [] _ =  []
mappings (x:xs) maps
  | x == "" = maps : mappings xs []
  | isDigit $ head x = mappings xs (readNumbers x : maps)
  | otherwise = mappings xs maps

solve1 :: [String] -> Int
solve1 str = let (sds, maps) = (seeds $ head str, mappings (tail str) [])
  in minLocation maps sds

compRange :: [Int] -> Int -> Bool
compRange [] _ = False
compRange (x:xs:xss) y
  | y >= x && y < x + xs = True
  | otherwise = compRange xss y

minBf :: [[(Int, Int, Int)]] -> [Int] -> Int -> Int
minBf maps x count 
  | compRange x $ mapRec maps count = count
  | otherwise = minBf maps x (count+1)
  
switch :: [(Int, Int, Int)] -> [(Int, Int, Int)]
switch [] = []
switch ((x, y, z):xs) = (y, x, z): switch xs

solve2 :: [String] -> Int
solve2 str = let (sds, maps) = (seeds $ head str, map switch (mappings (tail str) []))
  in minBf (reverse maps) sds 0

