module Day11 where

import Data.List (elemIndices)

padRows :: Int -> [String] -> [String]
padRows pads [] = []
padRows pads (x:xs) = if all (== '.') x then [x] ++ repPad (length x) pads ++ padRows pads xs else x:padRows pads xs
  where
    repPad n 1 = []
    repPad n p = replicate n '.' : repPad n (p - 1)

transp :: [String] -> [String]
transp str = if all null str then [] else map head str : transp (map tail str)

padCols :: Int -> [String] -> [String]
padCols pads str = transp . padRows pads $ transp str

padGrid :: Int -> [String] -> [String]
padGrid pads str = padCols pads $ padRows pads str

pairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

galaxies :: Int -> [String] -> [(Int, Int)]
galaxies _ [] = []
galaxies y (str:strs) = coords ++ galaxies (y + 1) strs
  where
    indices = elemIndices '#' str
    coords = zip indices (replicate (length indices) y)

sumPaths :: [(Int, Int)] -> Int
sumPaths x = sum $ map sumPath $ pairs x
  where
    sumPath ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

solve1 :: [String] -> Int
solve1 = sumPaths . galaxies 0 . padGrid 2

solve2 :: [String] -> Int
solve2 str = do
  let cheese = sumPaths . galaxies 0 $ padGrid 2 str
  let cheese2 = sumPaths . galaxies 0 $ padGrid 3 str
  cheese + (1000000 - 2) * (cheese2 - cheese) 
