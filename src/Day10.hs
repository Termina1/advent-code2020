module Day10
  ( day10,
    day10_2
  ) where

import Data.List.Split
import Data.Sort
import LibLib

countJoltageDifference :: [Int] -> (Int, Int, Int)
countJoltageDifference nums = foldl countDiffs (0, 0, 0) ((zip <*> tail) nums)
  where
    countDiffs (ones, thirds, twos) (fst, snd) = case snd - fst of
      1 -> (ones + 1, thirds, twos)
      3 -> (ones, thirds + 1, twos)
      _ -> (ones, thirds, twos + 1)

prepareJolts :: [Int] -> [Int]
prepareJolts nums = let sorted = sort nums in
  (0 : sorted) ++ [(last sorted) + 3]

day10 :: IO Int
day10 = do
  numbers <- parseIntArray "day10.txt"
  putStrLn (show $ prepareJolts numbers)
  let (x, y, z) = countJoltageDifference $ prepareJolts numbers in do
    putStrLn $ show (x, y, z)
    return (x * y)


toOnesSeq :: [Int] -> [Int]
toOnesSeq nums = snd $ foldl toOnesSeqHelper (0, []) ((zip <*> tail) nums)
  where
    toOnesSeqHelper :: (Int, [Int]) -> (Int, Int) -> (Int, [Int])
    toOnesSeqHelper (counter, acc) (cur, next) = if next - cur == 1 
      then (counter + 1, acc) 
      else if counter >= 2 
        then (0, (counter - 1) : acc)
        else (0, acc)

combinations :: [Int] -> Int
combinations nums = product $ map combinationsHelper nums
  where
    combinationsHelper 1 = 2
    combinationsHelper 2 = 4
    combinationsHelper 3 = 7

day10_2 :: IO Int
day10_2 = do
  numbers <- parseIntArray "day10.txt"
  return $ combinations $ toOnesSeq $ prepareJolts numbers

