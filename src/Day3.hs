module Day3
  ( day3,
    day3_2
  ) where

import Data.List.Split

parseMap :: String -> IO [[Int]]
parseMap filename = do
  contents <- readFile filename
  return $ map parseMapLine (filter (\n -> (length n) > 0) (splitOn "\n" contents))
  where
    parseMapLine :: [Char] -> [Int]
    parseMapLine list =  cycle $ map (\char -> if char == '#' then 1 else 0) list

walk :: [[Int]] -> (Int, Int) -> Int
walk tmap (right, down) = walkHelper (drop down tmap) 1 0
  where 
    walkHelper :: [[Int]] -> Int -> Int -> Int
    walkHelper [] depth acc = acc
    walkHelper tmap depth acc = walkHelper (drop down tmap) (depth + 1) (((head tmap) !! (right * depth)) + acc)

day3 :: IO Int
day3 = do 
  map <- parseMap "day3.txt"
  return $ walk map (3, 1)

day3_2 :: IO Int
day3_2 = do
  map <- parseMap "day3_2.txt"
  return $ (walk map (1, 1)) * (walk map (3, 1)) * (walk map (5, 1)) * (walk map (7, 1)) * (walk map (1, 2))
  