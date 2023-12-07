{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Day10 where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Ext

next :: [String] -> ((Int, Int), (Int, Int)) -> (Int, Int)
next str ((sx, sy), (x, y))
  | tile == 'J' && sx == x - 1 = (x, y - 1)
  | tile == 'J' && sx /= x - 1 = (x - 1, y)
  | tile == '|' && sy == y - 1 = (x, y + 1)
  | tile == '|' && sy /= y - 1 = (x, y - 1)
  | tile == '-' && sx == x - 1 = (x + 1, y)
  | tile == '-' && sx /= x - 1 = (x - 1, y)
  | tile == 'L' && sx == x + 1 = (x, y - 1)
  | tile == 'L' && sx /= x + 1 = (x + 1, y)
  | tile == '7' && sx == x - 1 = (x, y + 1)
  | tile == '7' && sx /= x - 1 = (x - 1, y)
  | tile == 'F' && sx == x + 1 = (x, y + 1)
  | tile == 'F' && sx /= x + 1 = (x + 1, y)
  where
    tile = str !! y !! x

same :: [(Int, Int)] -> Bool
same [] = True
same (x:xs) = all (== x) xs

minSteps :: [String] -> [(Int ,Int)] -> [(Int, Int)] -> Int
minSteps str prevDirs dirs = let nextTiles = zipWith (curry (next str)) prevDirs dirs
  in if same dirs then 1 else minSteps str dirs nextTiles + 1

connected :: [String] -> (Int, Int) -> (Int, Int) -> Bool
connected str (sx, sy) (x, y)
  | x < 0 || y < 0 || x > length (head str) || y > length (head str) = False
  | sx == x && sy == y - 1 = t == '|' || t == 'L' || t == 'J'
  | sx == x && sy == y + 1 = t == '|' && t == 'F'
  | sx == x - 1 && sy == y = t == '-' || t == 'J' || t == '7'
  | sx == x + 1 && sy == y = t == '-' || t == 'L'
  where
    t = str !! y !! x

getDirections :: [String] -> (Int, Int) -> [(Int, Int)]
getDirections str (x, y) = do
  let dirs = [(x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y)]
  let connectedTiles =  map (connected str (x,y)) dirs
  [t | (b, t) <- zip connectedTiles dirs, b]

findStart :: [String] -> Int -> Maybe (Int, Int)
findStart [] _ = Nothing
findStart (x:xs) y = case elemIndex 'S' x of
  Just i -> Just (i, y)
  Nothing -> findStart xs (y + 1)

solve1 :: [String] -> Int
solve1 str = do
  let (x, y) = fromMaybe (0, 0) $ findStart str 0
  let dirs = getDirections str (x, y)
  minSteps str [(x, y), (x, y)] dirs

buildPoly :: [String] -> (Int, Int) -> (Int, Int) -> [Point 2 Double :+ ()]
buildPoly str (sx, sy) (x, y) = let nextTiles = next str ((sx ,sy), (x, y))
  in if str !! y !! x == 'S' then [Point2 (fromIntegral x) (fromIntegral y) :+ ()] else (Point2 (fromIntegral x) (fromIntegral y) :+ ()) : buildPoly str (x, y) nextTiles

solve2 :: [String] -> Int
solve2 str = length $ filter id $ inside polygons points
   where
    (x, y) = fromMaybe (0, 0) $ findStart str 0
    dirs = getDirections str (x, y)
    polyPoints = buildPoly str (x, y) (head dirs)
    polygons = simpleFromPoints polyPoints
    points = [(\ (x, y) -> Point2 (fromIntegral x) (fromIntegral y)) (x, y) | x <- [0 .. (length (head str) - 1)], y <- [0 .. (length str - 1)]]
    inside ps = map (`insidePolygon` ps)
   
