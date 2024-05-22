{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import qualified Day1 (solve1, solve2)
import qualified Day2 (solve1, solve2)
import qualified Day3 (solve1, solve2)
import qualified Day4 (solve1, solve2)
import qualified Day5 (solve1, solve2)
import qualified Day6 (solve1, solve2)
import qualified Day7 (solve1, solve2)
import qualified Day8 (solve1, solve2)
import qualified Day9 (solve1, solve2)
import qualified Day10 (solve1, solve2)
import qualified Day11 (solve1, solve2)
import qualified Day12 (solve1, solve2)
import qualified Day13 (solve1, solve2)
import qualified Day14 (solve1, solve2)
import qualified Day15 (solve1, solve2)
import qualified Day16 (solve1, solve2)
import qualified Day18 (solve1, solve2)
import System.Console.CmdArgs

data DaysArgs = DaysArgs { day :: Integer, part :: Integer, file :: String } 
    deriving (Show, Data, Typeable)

dayArgs :: DaysArgs 
dayArgs = DaysArgs { day = def &= help "Set the day to evaluate" &= typ "INTEGER",
                     part = def &= help "Set the part" &= typ "INTEGER",
                     file = def &= help "Set file path" &= typ "STRING"   
                   }

solveDays :: Integer -> Integer -> String -> IO ()
solveDays day part file = 
  if null file
  then putStrLn "Input file is missing."
  else do
    input <- readFile file
    case (day, part) of
      (1, 1) -> putStrLn $ "Solution to day 1 part 1: " ++show (Day1.solve1 $ lines input)
      (1, 2) -> putStrLn $ "Solution to day 1 part 2: " ++show (Day1.solve2 $ lines input)
      (2, 1) -> putStrLn $ "Solution to day 2 part 1: " ++show (Day2.solve1 $ lines input)
      (2, 2) -> putStrLn $ "Solution to day 2 part 2: " ++show (Day2.solve2 $ lines input)
      (3, 1) -> putStrLn $ "Solution to day 3 part 1: " ++show (Day3.solve1 $ lines input)
      (3, 2) -> putStrLn $ "Solution to day 3 part 2: " ++show (Day3.solve2 $ lines input)
      (4, 1) -> putStrLn $ "Solution to day 4 part 1: " ++show (Day4.solve1 $ lines input)
      (4, 2) -> putStrLn $ "Solution to day 4 part 2: " ++show (Day4.solve2 $ lines input)
      (5, 1) -> putStrLn $ "Solution to day 5 part 1: " ++show (Day5.solve1 $ lines input)
      (5, 2) -> putStrLn $ "Solution to day 5 part 2: " ++show (Day5.solve2 $ lines input)
      (6, 1) -> putStrLn $ "Solution to day 6 part 1: " ++show (Day6.solve1 $ lines input)
      (6, 2) -> putStrLn $ "Solution to day 6 part 2: " ++show (Day6.solve2 $ lines input)
      (7, 1) -> putStrLn $ "Solution to day 7 part 1: " ++show (Day7.solve1 $ lines input)
      (7, 2) -> putStrLn $ "Solution to day 7 part 2: " ++show (Day7.solve2 $ lines input)
      (8, 1) -> putStrLn $ "Solution to day 8 part 1: " ++show (Day8.solve1 $ lines input)
      (8, 2) -> putStrLn $ "Solution to day 8 part 2: " ++show (Day8.solve2 $ lines input)
      (9, 1) -> putStrLn $ "Solution to day 9 part 1: " ++show (Day9.solve1 $ lines input)
      (9, 2) -> putStrLn $ "Solution to day 9 part 2: " ++show (Day9.solve2 $ lines input)
      (10, 1) -> putStrLn $ "Solution to day 10 part 1: " ++show (Day10.solve1 $ lines input)
      (10, 2) -> putStrLn $ "Solution to day 10 part 2: " ++show (Day10.solve2 $ lines input)
      (11, 1) -> putStrLn $ "Solution to day 11 part 1: " ++show (Day11.solve1 $ lines input)
      (11, 2) -> putStrLn $ "Solution to day 11 part 2: " ++show (Day11.solve2 $ lines input)
      (12, 1) -> putStrLn $ "Solution to day 12 part 1: " ++show (Day12.solve1 $ lines input)
      (12, 2) -> putStrLn $ "Solution to day 12 part 2: " ++show (Day12.solve2 $ lines input)
      (13, 1) -> putStrLn $ "Solution to day 13 part 1: " ++show (Day13.solve1 $ lines input)
      (13, 2) -> putStrLn $ "Solution to day 13 part 2: " ++show (Day13.solve2 $ lines input)
      (14, 1) -> putStrLn $ "Solution to day 14 part 1: " ++show (Day14.solve1 $ lines input)
      (14, 2) -> putStrLn $ "Solution to day 14 part 2: " ++show (Day14.solve2 $ lines input)
      (15, 1) -> putStrLn $ "Solution to day 15 part 1: " ++show (Day15.solve1 $ lines input)
      (15, 2) -> putStrLn $ "Solution to day 15 part 2: " ++show (Day15.solve2 $ lines input)
      (16, 1) -> putStrLn $ "Solution to day 16 part 1: " ++show (Day16.solve1 $ lines input)
      (16, 2) -> putStrLn $ "Solution to day 16 part 2: " ++show (Day16.solve2 $ lines input)
      (18, 1) -> putStrLn $ "Solution to day 18 part 1: " ++show (Day18.solve1 $ lines input)
      (18, 2) -> putStrLn $ "Solution to day 18 part 2: " ++show (Day18.solve2 $ lines input)
      _ -> putStrLn "Input not supported."

main :: IO ()
main = do
  args <- cmdArgs dayArgs
  solveDays (day args) (part args) (file args)
