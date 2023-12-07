module Day2 where

import Data.Char (isDigit, isSpace)

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

rgb :: (Integer, Integer, Integer)
rgb = (12, 13, 14)

validPick :: String -> Bool
validPick x = do
  let (r, g, b) = rgb
  let (first, rest) = span isDigit $ filter (not . isSpace) x
  let (num, col) = (read $ filter isDigit first, rest)
  case col of
    "green" -> num <= g
    "red" -> num <= r
    "blue" -> num <= b
    _ -> True

validPicks :: String -> Bool
validPicks "" = True
validPicks x = let (first, rest) = splitOnFirst ',' x
  in validPick first && validPicks rest

validGame :: String -> Bool
validGame "" = True
validGame s = let (first, rest) =  splitOnFirst ';' s
  in validPicks first && validGame rest

gameScores :: String -> Integer
gameScores x = let (first, rest) = splitOnFirst ':' x
  in if validGame rest then read $ filter isDigit first else 0

solve1 :: [String] -> Integer
solve1 = foldr ((+) . gameScores) 0

colorValue :: String -> (Integer, Integer, Integer)
colorValue x = do
  let (first, rest) = span isDigit $ filter (not . isSpace) x
  let (num, col) = (read $ filter isDigit first, rest)
  case col of
    "green" -> (0, num, 0)
    "red" -> (num, 0, 0)
    "blue" -> (0, 0, num)
    _ -> (0, 0, 0)

maxTuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
maxTuple (a, b, c) (d, e, f) = (max a d, max b e, max c f)

maxPick :: String -> (Integer, Integer, Integer)
maxPick "" = (0, 0, 0)
maxPick x = let (first, rest) = splitOnFirst ',' x
  in maxTuple (colorValue first) (maxPick rest)

maxPicks :: String -> (Integer, Integer, Integer)
maxPicks "" = (0, 0, 0)
maxPicks s = let (first, rest) =  splitOnFirst ';' s
  in maxTuple (maxPick first) (maxPicks rest)

minColorScores :: String -> Integer
minColorScores x =  a * b * c
  where
    (_, rest) = splitOnFirst ':' x
    (a, b, c) = maxPicks rest

solve2 :: [String] -> Integer
solve2  = foldr ((+) . minColorScores) 0
