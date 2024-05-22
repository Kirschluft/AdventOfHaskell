module Day15 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map

hash :: Int -> String -> Int
hash = foldl (\ s x -> 17 * (fromEnum x + s) `mod` 256)

solve1 :: [String] -> Int
solve1 = foldr ((+) . sum . map (hash 0) . splitOn ",") 0

type Box = Map.Map Int [(String, Int)]

splitOnFirst :: [Char] -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (`elem` sep) str in (a, drop 1 b)

insertBox :: Int -> (String, Int) -> Box -> Box
insertBox key (k, v) box =
  if null vals || all (\(s, _) -> s /= k) vals then Map.update (\x -> Just $ x ++ [(k, v)]) key box
  else Map.update (\x -> Just $ [if s == k then (s, v) else (s, i) | (s, i) <- x] ) key box
  where
    vals = Map.findWithDefault [] key box

action :: Box -> [String] -> Box
action box [] = box
action box (x:xs)
  | last x == '-' = action (Map.update (Just . filter ((/= k) . fst)) h  box) xs
  | otherwise = action (insertBox h (k, read v) box) xs
  where
    (k, v) = splitOnFirst "=-" x
    h = hash 0 k

sumMap :: [(Int, [(String, Int)])] -> Int
sumMap [] = 0
sumMap l = sum $ map (\(x, y) -> (x + 1) * sum (zipWith (*) [1..] (map snd y))) l

solve2 :: [String] -> Int
solve2 [] = 0
solve2 (x:xs) = do
  sumMap $ Map.toList box
  where
    m = Map.fromList [ (x, []) | x <- [0..255] ]
    s = splitOn "," x
    box = action m s

