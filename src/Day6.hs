module Day6 where

import Data.Char (isDigit)
import Data.List (group)

numWinners :: (Double, Double) -> Int
numWinners (x, y) = do
  let up = ceiling $ (- x - sqrt (x ^ 2 - 4 * (-1) * (-y))) / (-2) - 1
  let lw = floor $ (- x + sqrt (x ^ 2 - 4 * (-1) * (-y))) / (-2) + 1
  up - lw + 1

extractNumbers :: String -> [Double]
extractNumbers [] = []
extractNumbers str
  | isDigit (head str) = let (number, rest) = span isDigit str
                         in read number : extractNumbers rest
  | otherwise = extractNumbers (tail str)

format :: [String] -> [(Double, Double)]
format (x:xs) = do
  let t = extractNumbers x
  let d = extractNumbers $ head xs
  glue t d
  where
    glue [] [] = []
    glue (a:as) (b:bs) = (a, b):glue as bs

solve1 :: [String] -> Int
solve1 = product . map numWinners . format

format2 :: [String] -> (Double, Double)
format2 (x:xs) = (read $ filter isDigit x, read $ filter isDigit $ head xs)

solve2 :: [String] -> Int
solve2 = numWinners . format2
