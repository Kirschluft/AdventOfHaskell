module Day14 where

rollLeft :: (Int, Int) -> String -> String
rollLeft (x, 0) [] = replicate x 'O'
rollLeft (0, y) [] = replicate y '.'
rollLeft (x, y) [] = replicate x 'O' ++ replicate y '.'
rollLeft (o, p) (x:xs)
  | x == 'O' = rollLeft (o + 1, p) xs
  | x == '.' = rollLeft (o, p + 1) xs
  | x == '#' = replicate o 'O' ++ replicate p '.' ++ ['#'] ++ rollLeft (0, 0) xs
  | otherwise = rollLeft (o, p) xs

transp :: [String] -> [String]
transp str = if any null str then [] else map head str : transp (map tail str)

rollNorth :: [String] -> [String]
rollNorth = map (rollLeft (0, 0))

scoreRow :: String -> Int -> Int
scoreRow [] _ = 0
scoreRow (x:xs) score = if x == 'O' then score + scoreRow xs (score -1) else scoreRow xs (score - 1)

score :: [String] -> Int
score = foldr (\ x -> (+) (scoreRow x (length x))) 0

solve1 :: [String] -> Int
solve1 = score . rollNorth . transp

rollWest :: [String] -> [String]
rollWest = map (rollLeft (0, 0))

rollEast :: [String] -> [String]
rollEast = map (reverse . rollLeft (0, 0) . reverse)

rollSouth :: [String] -> [String]
rollSouth str = reverse $ rollEast $ reverse str

-- TODO: other approach, e.g. sliding window/floyd
cycleDetection :: [String] -> Int -> Int -> Int
cycleDetection str prev i
  | i == 999 = currentScore
  | otherwise = cycleDetection spin currentScore (i + 1)
  where
    spin = rollEast $ transp $ rollSouth $ transp $ rollWest $ transp$ rollNorth $ transp str
    currentScore = score $ transp spin

solve2 :: [String] -> Int
solve2 str = cycleDetection str 0 0
