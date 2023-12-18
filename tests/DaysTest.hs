module Main where

import Test.HUnit
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8
import qualified Day9 as D9
import qualified Day10 as D10
import qualified Day11 as D11
import qualified Day12 as D12
import qualified Day13 as D13
import qualified Day14 as D14
import qualified Day15 as D15
import qualified Day16 as D16
import qualified Day18 as D18

test1Day1 :: [String] -> Test
test1Day1 input = TestCase(assertEqual "Test day1 part 1: " 142 (D1.solve1 input))

test2Day1 :: [String] -> Test
test2Day1 input = TestCase(assertEqual "Test day1 part 2: " 281 (D1.solve2 input))

test1Day2 :: [String] -> Test
test1Day2 input = TestCase(assertEqual "Test day2 part 1: " 8 (D2.solve1 input))

test2Day2 :: [String] -> Test
test2Day2 input = TestCase(assertEqual "Test day2 part 2: " 2286 (D2.solve2 input))

test1Day3 :: [String] -> Test
test1Day3 input = TestCase(assertEqual "Test day3 part 1: " 4361 (D3.solve1 input))

test2Day3 :: [String] -> Test
test2Day3 input = TestCase(assertEqual "Test day3 part 2: " 467835 (D3.solve2 input))

test1Day4 :: [String] -> Test
test1Day4 input = TestCase(assertEqual "Test day4 part 1: " 13 (D4.solve1 input))

test2Day4 :: [String] -> Test
test2Day4 input = TestCase(assertEqual "Test day4 part 2: " 30 (D4.solve2 input))

test1Day5 :: [String] -> Test
test1Day5 input = TestCase(assertEqual "Test day5 part 1: " 35 (D5.solve1 input))

test2Day5 :: [String] -> Test
test2Day5 input = TestCase(assertEqual "Test day5 part 2: " 46 (D5.solve2 input))

test1Day6 :: [String] -> Test
test1Day6 input = TestCase(assertEqual "Test day6 part 1: " 288 (D6.solve1 input))

test2Day6 :: [String] -> Test
test2Day6 input = TestCase(assertEqual "Test day6 part 2: " 71503 (D6.solve2 input))

test1Day7 :: [String] -> Test
test1Day7 input = TestCase(assertEqual "Test day7 part 1: " 6440 (D7.solve1 input))

test2Day7 :: [String] -> Test
test2Day7 input = TestCase(assertEqual "Test day7 part 2: " 5905 (D7.solve2 input))

test1Day8 :: [String] -> Test
test1Day8 input = TestCase(assertEqual "Test day8 part 1: " 2 (D8.solve1 input))

test2Day8 :: [String] -> Test
test2Day8 input = TestCase(assertEqual "Test day8 part 2: " 6 (D8.solve2 input))

test1Day9 :: [String] -> Test
test1Day9 input = TestCase(assertEqual "Test day9 part 1: " 114 (D9.solve1 input))

test2Day9 :: [String] -> Test
test2Day9 input = TestCase(assertEqual "Test day9 part 2: " 2 (D9.solve2 input))

test1Day10 :: [String] -> Test
test1Day10 input = TestCase(assertEqual "Test day10 part 1: " 8 (D10.solve1 input))

test2Day10 :: [String] -> Test
test2Day10 input = TestCase(assertEqual "Test day10 part 2: " 4 (D10.solve2 input))

test22Day10 :: [String] -> Test
test22Day10 input = TestCase(assertEqual "Test day10 part 2_2: " 8 (D10.solve2 input))

test1Day11 :: [String] -> Test
test1Day11 input = TestCase(assertEqual "Test day11 part 1: " 374 (D11.solve1 input))

test2Day11 :: [String] -> Test
test2Day11 input = TestCase(assertEqual "Test day11 part 2: " 82000210 (D11.solve2 input))

test1Day12 :: [String] -> Test
test1Day12 input = TestCase(assertEqual "Test day12 part 1: " 21 (D12.solve1 input))

test2Day12 :: [String] -> Test
test2Day12 input = TestCase(assertEqual "Test day12 part 2: " 525152 (D12.solve2 input))

test1Day13 :: [String] -> Test
test1Day13 input = TestCase(assertEqual "Test day13 part 1: " 405 (D13.solve1 input))

test2Day13 :: [String] -> Test
test2Day13 input = TestCase(assertEqual "Test day13 part 2: " 400 (D13.solve2 input))

test1Day14 :: [String] -> Test
test1Day14 input = TestCase(assertEqual "Test day14 part 1: " 136 (D14.solve1 input))

test2Day14 :: [String] -> Test
test2Day14 input = TestCase(assertEqual "Test day14 part 2: " 64 (D14.solve2 input))

test1Day15 :: [String] -> Test
test1Day15 input = TestCase(assertEqual "Test day15 part 1: " 1320 (D15.solve1 input))

test2Day15 :: [String] -> Test
test2Day15 input = TestCase(assertEqual "Test day15 part 2: " 145 (D15.solve2 input))

test1Day16 :: [String] -> Test
test1Day16 input = TestCase(assertEqual "Test day16 part 1: " 46 (D16.solve1 input))

test2Day16 :: [String] -> Test
test2Day16 input = TestCase(assertEqual "Test day16 part 2: " 51 (D16.solve2 input))

test1Day18 :: [String] -> Test
test1Day18 input = TestCase(assertEqual "Test day18 part 1: " 62 (D18.solve1 input))

test2Day18 :: [String] -> Test
test2Day18 input = TestCase(assertEqual "Test day18 part 2: " 952408144115 (D18.solve2 input))

main :: IO ()
main = do
  file1Day1 <- readFile "tests/day1_1.input"
  file2Day1 <- readFile "tests/day1_2.input"
  file1Day2 <- readFile "tests/day2_1.input"
  file2Day2 <- readFile "tests/day2_2.input"
  file1Day3 <- readFile "tests/day3_1.input"
  file2Day3 <- readFile "tests/day3_2.input"
  file1Day4 <- readFile "tests/day4_1.input"
  file2Day4 <- readFile "tests/day4_2.input"
  file1Day5 <- readFile "tests/day5_1.input"
  file2Day5 <- readFile "tests/day5_2.input"
  fileDay6 <- readFile "tests/day6.input"
  fileDay7 <- readFile "tests/day7.input"
  file1Day8 <- readFile "tests/day8_1.input"
  file2Day8 <- readFile "tests/day8_2.input"
  fileDay9 <- readFile "tests/day9.input"
  file1Day10 <- readFile "tests/day10_1.input"
  file2Day10 <- readFile "tests/day10_2.input"
  file22Day10 <- readFile "tests/day10_2_2.input"
  file1Day11 <- readFile "tests/day11_1.input"
  file2Day11 <- readFile "tests/day11_2.input"
  file1Day12 <- readFile "tests/day12_1.input"
  file2Day12 <- readFile "tests/day12_2.input"
  file1Day13 <- readFile "tests/day13_1.input"
  file2Day13 <- readFile "tests/day13_2.input"
  file1Day14 <- readFile "tests/day14_1.input"
  file2Day14 <- readFile "tests/day14_2.input"
  file1Day15 <- readFile "tests/day15_1.input"
  file2Day15 <- readFile "tests/day15_2.input"
  file1Day16 <- readFile "tests/day16_1.input"
  file2Day16 <- readFile "tests/day16_2.input"
  file1Day18 <- readFile "tests/day18_1.input"
  file2Day18 <- readFile "tests/day18_2.input"
  runTestTTAndExit $ TestList [ 
                        test1Day1 $ lines file1Day1,
                        test2Day1 $ lines file2Day1,
                        test1Day2 $ lines file1Day2,
                        test2Day2 $ lines file2Day2,
                        test1Day3 $ lines file1Day3,
                        test2Day3 $ lines file2Day3,
                        test1Day4 $ lines file1Day4,
                        test2Day4 $ lines file2Day4,
                        test1Day5 $ lines file1Day5,
                        test2Day5 $ lines file2Day5,
                        test1Day6 $ lines fileDay6,
                        test2Day6 $ lines fileDay6,
                        test1Day7 $ lines fileDay7,
                        test2Day7 $ lines fileDay7,
                        test1Day8 $ lines file1Day8,
                        test2Day8 $ lines file2Day8,
                        test1Day9 $ lines fileDay9,
                        test2Day9 $ lines fileDay9,
                        test1Day10 $ lines file1Day10,
                        test2Day10 $ lines file2Day10,
                        test22Day10 $ lines file22Day10,
                        test1Day11 $ lines file1Day11,
                        test2Day11 $ lines file2Day11,
                        test1Day12 $ lines file1Day12,
                        test2Day12 $ lines file2Day12,
                        test1Day13 $ lines file1Day13,
                        test2Day13 $ lines file2Day13,
                        test1Day14 $ lines file1Day14,
                        test2Day14 $ lines file2Day14,
                        test1Day15 $ lines file1Day15,
                        test2Day15 $ lines file2Day15,
                        test1Day16 $ lines file1Day16,
                        test2Day16 $ lines file2Day16,
                        test1Day18 $ lines file1Day18,
                        test2Day18 $ lines file2Day18
                       ]
