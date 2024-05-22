module Day9 where

diff :: [Int] -> [Int]
diff [x] = []
diff (x:xs) = (head xs - x) : diff xs

predict :: ([Int] -> Int) -> (Int -> Int -> Int) -> [Int] -> Int
predict lf op x
  | all (== 0) x = 0
  | otherwise = op (lf x) (predict lf op (diff x))

solve1 :: [String] -> Int
solve1 = sum . map (predict last (+) . map read . words)

solve2 :: [String] -> Int
solve2 = sum . map (predict head (-) . map read . words)
