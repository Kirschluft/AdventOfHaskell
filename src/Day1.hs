module Day1 where

import Data.Char (isDigit, digitToInt)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Bifunctor

concatDigits :: Int -> Int -> Int
concatDigits x y = x * 10 + y

lineSum :: String -> Maybe Int
lineSum str = let digits = map digitToInt $ filter isDigit str
    in case digits of
    [] -> Nothing
    [x] -> Just $ concatDigits x x
    xs -> Just $ concatDigits (head xs) (last xs)

solve1 :: [String] -> Int
solve1 = sum . mapMaybe lineSum

mapDigits :: [(T.Text, T.Text)]
mapDigits = map (Data.Bifunctor.bimap T.pack T.pack)
              [("one", "o1ne"), ("two", "t2wo"), ("three", "t3hree"),
              ("four", "f4our"), ("five", "f5ive"), ("six", "s6ix"),
              ("seven", "s7even"), ("eight", "e8ight"), ("nine", "n9ine")]

replaceNumbers :: String -> String
replaceNumbers = T.unpack . convert mapDigits . T.pack

convert :: [(T.Text, T.Text)] -> T.Text -> T.Text
convert [] text = text
convert ((old,new):xs) text = convert xs $ T.replace old new text

solve2 :: [String] -> Int
solve2 = sum . mapMaybe (lineSum . replaceNumbers)

