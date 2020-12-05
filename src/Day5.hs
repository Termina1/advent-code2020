module Day5
  ( day5, day5_2
  ) where

import Data.List.Split
import Debug.Trace
import Data.Sort

search :: Char -> String -> Float
search lower str = foldl (\acc x -> acc * 2 + (if x == lower then 0 else 1)) 0 str

parseBoardingPass :: String -> (Float, Float)
parseBoardingPass bpstring = (searchRow (take 7 bpstring), searchColumn (drop 7 bpstring))
  where
    searchRow = search 'F'
    searchColumn = search 'L'

parseSeatId :: String -> Float
parseSeatId bpstring = row * 8 + column
  where
    (row, column) = parseBoardingPass bpstring

findSeat :: [Float] -> Float
findSeat tickets = let (max, min) = (maximum tickets, minimum tickets) in
  (min + max) / 2 * (max - min + 1) - (sum tickets)

day5 :: IO Float
day5 = do 
  contents <- readFile "day5.txt"
  return $ maximum $  map parseSeatId (splitOn "\n" contents)

day5_2 :: IO Float
day5_2 = do 
  contents <- readFile "day5_2.txt"
  return $ findSeat (map parseSeatId (splitOn "\n" contents))
