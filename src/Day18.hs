module Day18 where

next :: (Char, Int) -> (Int, Int) -> (Int, Int)
next ('R', moves) (x, y) = (x + moves, y)
next ('L', moves) (x, y) = (x - moves, y)
next ('D', moves) (x, y) = (x, y + moves)
next ('U', moves) (x, y) = (x, y - moves)

buildPoly :: [(Char, Int)] -> (Int, Int) -> [(Int, Int)]
buildPoly [] _ = []
buildPoly ((cmd, moves):xs) (x, y) = let (edgex, edgey) = next (cmd, moves) (x, y)
  in (edgex, edgey) : buildPoly xs (edgex, edgey)

parse :: [String] -> [(Char, Int)]
parse [] = []
parse (x:xs) = let (cmd:move:_) = words x
  in (head cmd, read move) : parse xs

shoes :: [(Int, Int)] -> Int
shoes [x] = 0
shoes ((x1, y1):(x2, y2):xss) = y2 * x1 - x2 * y1 + shoes ((x2, y2):xss)

solve1 :: [String] -> Int
solve1 str = shoes polyPoints `div` 2 + sum (map snd cmds) `div` 2 + 1
  where
    cmds = parse str
    polyPoints = (0, 0) : buildPoly cmds (0, 0)

parseHex :: [String] -> [(Char, Int)]
parseHex [] = []
parseHex (x:xs) = do
  let (_:_:hex:_) = words x
  let cmd = init (tail hex)
  (dir cmd, read ("0x" ++ tail (init cmd))) : parseHex xs
  where
    dir cmd
      | last cmd == '0' = 'R'
      | last cmd == '1' = 'D'
      | last cmd == '2' = 'L'
      | last cmd == '3' = 'U'

solve2 :: [String] -> Int
solve2 str = shoes polyPoints `div` 2 + sum (map snd cmds) `div` 2 + 1
  where
    cmds = parseHex str
    polyPoints = (0, 0) : buildPoly cmds (0, 0)

