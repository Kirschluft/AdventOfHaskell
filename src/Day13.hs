module Day13 where

divide :: [String] -> [[String]]
divide = foldr glue [[]]
  where
    glue "" ([]:x) = []:x
    glue "" x = []:x
    glue str (x:xs) = (str:x):xs

horizontalStarts :: Int -> [String] -> [Int]
horizontalStarts _ [] = []
horizontalStarts _ [x] = []
horizontalStarts i (x:xs:xss) = if x == xs then i:horizontalStarts (i + 1) (xs:xss) else horizontalStarts (i + 1) (xs:xss)

horizontalReflection :: [String] -> (Int, Int) -> Bool
horizontalReflection str (y1, y2)
  | y1 < 0 || y2 < 0 || y1 >= length str || y2 >= length str = True
  | str !! y1 == str !! y2 = horizontalReflection str (y1 - 1, y2 + 1)
  | otherwise = False

transp :: [String] -> [String]
transp str = if all null str then [] else map head str : transp (map tail str)

solve1 :: [String] -> Int
solve1 str = do
  let divided = divide str
  let horStart = map (horizontalStarts 0) divided
  let horRef = zipWith (curry (apply 100)) divided horStart
  let vertDivided = map transp divided
  let vertStart = map (horizontalStarts 0) vertDivided
  let vertRef = zipWith (curry (apply 1)) vertDivided vertStart
  sum horRef + sum vertRef
  where
    apply :: Int -> ([String], [Int]) -> Int
    apply factor (strs, ints) = sum $ map (\i -> if horizontalReflection strs (i, i + 1) then (i + 1) * factor else 0) ints

horizontalStarts2 :: Int -> [String] -> [Int]
horizontalStarts2 _ [] = []
horizontalStarts2 _ [x] = []
horizontalStarts2 i (x:xs:xss) = if offByAtMostOne x xs <= 1 then i:horizontalStarts2 (i + 1) (xs:xss) else horizontalStarts2 (i + 1) (xs:xss)

horizontalReflection2 :: [String] -> (Int, Int) -> Int -> Bool
horizontalReflection2 str (y1, y2) i
  | (y1 < 0 || y2 < 0 || y1 >= length str || y2 >= length str) && i == 1 = True
  | y1 < 0 || y2 < 0 || y1 >= length str || y2 >= length str = False
  | one <=1 = horizontalReflection2 str (y1 - 1, y2 + 1) (i + one)
  | otherwise = False
  where
    one = offByAtMostOne (str !! y1) (str !! y2)

offByAtMostOne :: String -> String -> Int
offByAtMostOne x y = length $ filter not $ zipWith (==) x y

solve2 :: [String] -> Int
solve2 str = do
  let divided = divide str
  let horStart = map (horizontalStarts2 0) divided
  let horRef = zipWith (curry (apply 100)) divided horStart
  let vertDivided = map transp divided
  let vertStart = map (horizontalStarts2 0) vertDivided
  let vertRef = zipWith (curry (apply 1)) vertDivided vertStart
  sum horRef + sum vertRef
  where
    apply :: Int -> ([String], [Int]) -> Int
    apply factor (strs, ints) = sum $ map (\i -> if horizontalReflection2 strs (i, i + 1) 0 then (i + 1) * factor else 0) ints
