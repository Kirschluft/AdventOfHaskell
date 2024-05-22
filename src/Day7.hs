module Day7 where

import Data.List (sort, group, sortBy)
import Data.Char (isDigit)
import Data.Ord (comparing, Down (Down))

type Power =  Int
type Rank = Int
type Bid = Int
type Hand = String
type Card = Char

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

cardPower :: Card -> Power
cardPower card
  | isDigit card = read [card]
  | card == 'A' = 123456789
  | card == 'K' = 12345678
  | card == 'Q' = 1234567
  | card == 'J' = 123456
  | card == 'T' = 12345
  | otherwise = 0

handPower :: [Power] -> Power
handPower (5:_) = 8
handPower (4:_) = 6
handPower (3:2:_) = 5
handPower (3:_) = 4
handPower (2:2:_) = 3
handPower (2:_) = 2
handPower _ = 0

getPower :: String -> Power
getPower =  handPower . sortBy (comparing Data.Ord.Down) . map length . group . sort . getHand

evalHand :: (Card -> Power) -> Hand -> Hand -> Ordering
evalHand cp [] [] = EQ
evalHand cp (cardx:xs) (cardy:ys)
  | cpx == cpy = evalHand cp xs ys
  | otherwise = cpx `compare` cpy
  where
    cpx = cp cardx
    cpy = cp cardy

compHands :: String -> String -> Ordering
compHands [] [] = EQ
compHands (x:xs) (y:ys)
  | px == py = evalHand cardPower (getHand (x:xs)) (getHand (y:ys))
  | otherwise = px `compare` py
  where
    px = getPower (x:xs)
    py = getPower (y:ys)

getHand :: String -> Hand
getHand = fst . splitOnFirst ' '

getBid :: String -> Bid
getBid = read . snd . splitOnFirst ' '

solve1 :: [String] -> Int
solve1 = sum . zipWith (*) [1..] . map getBid . sortBy compHands

cardPowerJoker :: Card -> Power
cardPowerJoker card
  | isDigit card = read [card]
  | card == 'A' = 123456789
  | card == 'K' = 12345678
  | card == 'Q' = 1234567
  | card == 'T' = 12345
  | otherwise = 0

casualHead :: a -> [a] -> a
casualHead a [] = a
casualHead a (x:_) = x

getPowerJoker :: String -> Power
getPowerJoker x = do
  let groups = sortBy (comparing Data.Ord.Down) . map length . group . sort . getHand $ filter (/= 'J') x
  
  handPower (casualHead 0 groups + length (filter (== 'J') x):tail groups)

compHandsJoker :: String -> String -> Ordering
compHandsJoker [] [] = EQ
compHandsJoker (x:xs) (y:ys)
  | px == py = evalHand cardPowerJoker (getHand (x:xs)) (getHand (y:ys))
  | otherwise = px `compare` py
  where
    px = getPowerJoker (x:xs)
    py = getPowerJoker (y:ys)

solve2 :: [String] -> Int
solve2 = sum . zipWith (*) [1..] . map getBid . sortBy compHandsJoker
