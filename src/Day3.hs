module Day3 where

import Data.Char (isDigit)
import Data.List (elemIndices, nub)

groupAdj :: [Int] -> [[Int]]
groupAdj = foldr glue []
  where
    glue x [] = [[x]]
    glue x (y:ys) = if x + 1 == head y then (x:y):ys else [x]:y:ys

extractNumber :: String -> Int -> Int
extractNumber "" idx = 0
extractNumber str idx = read . takeWhile isDigit . drop idx $ str

isValidSymbol :: String -> Int -> Bool
isValidSymbol str i
  | i < 0 || i >= length str = False
  | noSymbol = False
  | otherwise = True
  where
    noSymbol = let ch = str !! i in ch == '.' || isDigit ch

isValidNumber :: (String, String, String) -> Int -> Bool
isValidNumber (x, y , z) i
  | isValidSymbol y (i - 1) = True
  | isValidSymbol y (i + 1) = True
  | isValidSymbol x i = True
  | isValidSymbol x (i - 1) = True
  | isValidSymbol x (i + 1) = True
  | isValidSymbol z i = True
  | isValidSymbol z (i - 1) = True
  | isValidSymbol z (i + 1) = True
  | otherwise = False

findNumbers :: String -> [Int]
findNumbers str = [i | (c, i) <- zip str [0..], isDigit c]

anyValid :: (String, String, String) -> [Int] -> Bool
anyValid str = foldr ((||) . isValidNumber str) False

findMatchNumber :: (String, String, String) -> [Int] -> [Int]
findMatchNumber (x, y, z) numbers = do
  let pairs = groupAdj numbers
  let isValidNumber = map (anyValid (x, y, z)) pairs
  let validNumbersFirstIndex = map (head . snd) $ filter fst $ zip isValidNumber pairs
  map (extractNumber y) validNumbersFirstIndex

evaluateNumbers :: (String, String, String) -> Int
evaluateNumbers (x, y, z) = sum $ findMatchNumber (x, y, z) (findNumbers y)

adjSum :: [String] -> Int
adjSum [] = 0
adjSum [x] = 0
adjSum [x, y] = 0
adjSum (x:y:z:xs) = evaluateNumbers (x, y, z) + adjSum (y:z:xs)

pad :: [String] -> [String]
pad x = do
  let len = length (head x)
  let sufprefix = replicate len '.'
  sufprefix : x ++ [sufprefix]

solve1 :: [String] -> Int
solve1 = adjSum . pad

contains :: [[Int]] -> Int -> [[Int]]
contains x i = filter (i `elem`) x

numbersAdj :: (String, [Int]) -> [Int]
numbersAdj (str, i) = map (extractNumber str . head) $ nub $ concatMap (contains $ groupAdj $ findNumbers str) i

gearProduct :: (String, String, String) -> Int -> Int
gearProduct (x, y, z) i
  | length adjNumbers == 2 = product adjNumbers
  | otherwise = 0
  where
    adjNumbers = concatMap numbersAdj [ (x, [i - 1, i, i + 1]), (y, [i - 1, i + 1]), (z, [i - 1, i, i + 1])]

gearScores :: (String, String, String) -> Int
gearScores (x, y, z) = sum $ map (gearProduct (x, y, z)) $ elemIndices '*' y

gearRatioSum :: [String] -> Int
gearRatioSum [] = 0
gearRatioSum [x] = 0
gearRatioSum [x, y] = 0
gearRatioSum (x:y:z:xs) = gearScores (x, y, z) + gearRatioSum (y:z:xs)

solve2 :: [String] -> Int
solve2 = gearRatioSum . pad
