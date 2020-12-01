module Lib
  ( someFunc
  ) where

import Data.List.Split
import Data.IntMap

someFunc :: IO ()
someFunc = do 
  answer <- day1
  putStrLn (show answer)


parseIntArray :: String -> IO [Int]
parseIntArray filename = do 
  contents <- readFile filename
  return $ Prelude.map read (Prelude.filter (\n -> (length n) > 0) (splitOn "\n" contents))


find2Sum :: [Int] -> IntMap Int -> Int
find2Sum [] hm = 0
find2Sum (head : tail) hm =
  if member head hm
    then (findWithDefault 0 head hm) * head
    else find2Sum tail (insert (2020 - head) head hm)

day1 :: IO Int
day1 = do 
  array <- parseIntArray "day1.txt"
  return (find2Sum array empty)
