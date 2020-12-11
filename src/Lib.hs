module Lib
  ( someFunc
  ) where

import Data.IntMap
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import LibLib

someFunc :: IO ()
someFunc = do
  answer <- day11_2
  putStrLn (show answer)

find2Sum :: [Int] -> Int -> IntMap Int -> Int
find2Sum [] target hm = 0
find2Sum (head : tail) target hm =
  if member head hm
    then (findWithDefault 0 head hm) * head
    else find2Sum tail target (insert (target - head) head hm)

find3sum :: [Int] -> [Int] -> Int
find3sum [] _ = 0
find3sum (head : tail) start =
  let sum2 = find2Sum (start ++ tail) (2020 - head) empty in
  if sum2 > 0 then sum2 * head else (find3sum tail (start ++ [head]))

day1 :: IO Int
day1 = do
  array <- parseIntArray "day1.txt"
  return (find2Sum array 2020 empty)

day1_2 :: IO Int
day1_2 = do
  array <- parseIntArray "day1_2.txt"
  return (find3sum array [])
