module Day16 where

import qualified Data.HashSet as HashSet
import Data.List (nub)

type Tiles = HashSet.HashSet ((Int, Int), (Int, Int))

move :: [String] -> Tiles -> (Int, Int) -> (Int, Int) -> Tiles
move str tiles (x, y) (dx, dy)
  | y < 0 || x < 0 || y >= length str || x >= length (str !! y) = tiles
  | ((x, y), (dx, dy)) `HashSet.member` tiles = tiles
  | tile == '.' = move str newTiles (x + dx, y + dy) (dx, dy)
  | tile == '|' && (dy == 1 || dy == -1) = move str newTiles (x + dx, y + dy) (dx, dy)
  | tile == '|' && (dx == 1 || dx == -1) = move str (move str newTiles (x, y + 1) (0, 1)) (x, y - 1) (0, -1)
  | tile == '-' && (dy == 1 || dy == -1) = move str (move str newTiles (x + 1, y) (1, 0)) (x - 1, y) (-1, 0) 
  | tile == '-' && (dx == 1 || dx == -1) = move str newTiles (x + dx, y + dy) (dx, dy)
  | tile == '/' = move str newTiles (x -dy, y - dx) (-dy, -dx)
  | tile == '\\' = move str newTiles (x + dy, y + dx) (dy, dx)
  where
    tile = str !! y !! x
    newTiles = HashSet.insert ((x, y), (dx, dy)) tiles

energizedTiles :: [String] -> ((Int, Int), (Int, Int)) -> Int
energizedTiles str ((x, y), (dx, dy)) = length $ nub $ HashSet.toList $ HashSet.map (\((a, b), (c, d)) -> (a, b)) $ move str HashSet.empty (x, y) (dx, dy)

solve1 :: [String] -> Int
solve1 str = energizedTiles str ((0, 0), (1, 0))

solve2 :: [String] -> Int
solve2 str = maximum $ map (energizedTiles str) gridStart
  where
    lenX = length (head str)
    lenY = length str
    gridStart = [ ((x, 0), (0, 1)) | x <- [0..lenX] ] ++
                [ ((x, lenY), (0, -1)) | x <- [0..lenX] ] ++
                [ ((0, y), (1, 0)) | y <- [0..lenY] ] ++
                [ ((lenX, y), (-1, 0)) | y <- [0..lenY] ]
                
