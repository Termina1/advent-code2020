module Day6
  ( day6,
    day6_2 
  ) where

import Data.List.Split
import Data.Set (insert, empty, size, intersection)

parseAnswers :: String -> IO [[String]]
parseAnswers filename = do
  contents <- readFile filename
  return $ map words $ splitOn "\n\n" contents

countYesAnswers :: ([String] -> Int) -> [[String]] -> Int
countYesAnswers countGroup = sum . map countGroup
 
countAnyYesAnswer :: [String] -> Int
countAnyYesAnswer = size . foldl (foldr insert) empty

countAllYesAnswer :: [String] -> Int
countAllYesAnswer = size . (foldl1 intersection) . map (foldr insert empty)

day6 :: IO Int
day6 = do 
  answers <- parseAnswers "day6.txt"
  return $ countYesAnswers countAnyYesAnswer answers

day6_2 :: IO Int
day6_2 = do 
  answers <- parseAnswers "day6_2.txt"
  return $ countYesAnswers countAllYesAnswer answers
