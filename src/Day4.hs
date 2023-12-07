module Day4 where

import qualified Data.IntMap.Strict as IntMap
import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.Text as T

type Index = Int
type Count = Int
type Cards = IntMap.IntMap Count

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

contains :: [Int] -> [Int] -> [Int]
contains w = concatMap (\ c -> nub (filter (== c) w)) 

twoExp :: Int -> Int
twoExp x
  | x <= 0 = 0
  | otherwise = 2 ^ (x - 1)

readNumbers :: String -> [Int]
readNumbers "" = []
readNumbers x = do
  let (y, ys) = splitOnFirst ' ' $ T.unpack $ T.strip $ T.pack x
  (read y ::Int):readNumbers ys

winners :: [String] -> Int
winners [] = 0
winners (x:xs) = do
  let (w, c) = splitOnFirst '|' x
  let (_, ws) = splitOnFirst ':' w
  twoExp (length $ contains (readNumbers c) (readNumbers ws)) + winners xs

solve1 :: [String] -> Int
solve1 = winners

incrementCardCount :: Index -> Int -> Cards -> Cards
incrementCardCount = IntMap.insertWith (+)

incrementCardCountRec :: Index -> Int -> Count -> Cards -> Cards
incrementCardCountRec index value count cards
  | count <= 0 = cards
  | otherwise = incrementCardCountRec (index + 1) value (count - 1) (incrementCardCount index value cards)

getCardCount :: Index -> Cards -> Count
getCardCount = IntMap.findWithDefault 0

initCards :: Int -> Cards -> Cards
initCards count cards 
  | count < 0 = cards
  | otherwise = initCards (count - 1) $ incrementCardCount count 1 cards

scratchCardsTotal :: [String] -> Index -> Cards -> Int
scratchCardsTotal [] index cards = 0
scratchCardsTotal (x:xs) index cards = do
  let (w, c) = splitOnFirst '|' x
  let (_, ws) = splitOnFirst ':' w
  let wins = length (contains (readNumbers c) (readNumbers ws))
  let count = getCardCount index cards
  let inc = incrementCardCountRec (index + 1) count wins cards
  getCardCount index inc + scratchCardsTotal xs (index + 1) inc

solve2 :: [String] -> Int
solve2 str = scratchCardsTotal  str 0 $ initCards (length str - 1) IntMap.empty
