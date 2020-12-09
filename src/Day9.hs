module Day9
  ( day9,
    day9_2
  ) where

import Data.List.Split
import qualified Data.IntMap as IMap
import Debug.Trace

lastNum :: Int
lastNum = 25

parseNumList :: String -> IO [Int]
parseNumList filename = do
  content <- readFile filename
  return $ map read (splitOn "\n" content)

generateSumList :: [Int] -> Int -> [Int]
generateSumList nums num = foldl (\acc el -> acc ++ [el + num]) [] nums

initN :: [Int] -> [Int]
initN nums = initHelper nums []
  where
    initHelper :: [Int] -> [Int] -> [Int]
    initHelper [] start = []
    initHelper (el : end) start = (generateSumList (start ++ end) el) ++ (initHelper end (start ++ [el]))

findNonSum :: [Int] -> [Int] -> [Int] -> Int
findNonSum (el : t) last cache = if elem el cache 
  then let nwlast = (tail last) ++ [el] in
    findNonSum t nwlast (updateCache el nwlast cache) 
  else el
  where
    updateCache :: Int -> [Int] -> [Int] -> [Int]
    updateCache el last nums = (drop lastNum nums) ++ (generateSumList last el)

findCSum :: Int -> [Int] -> Maybe (Int, Int)
findCSum target [] = Nothing
findCSum target nums = findCSumHelper target nums IMap.empty 0
  where
    findCSumHelper :: Int -> [Int] -> (IMap.IntMap Int) -> Int -> Maybe (Int, Int)
    findCSumHelper target [] hash sum = Nothing
    findCSumHelper target (x : tail) hash sum = case IMap.lookup (sum - target) hash of
      Nothing -> findCSumHelper target tail (IMap.insert (sum + x) ((IMap.size hash) + 1) hash) (sum + x)
      Just i -> if (IMap.size hash - i) >= 2
        then Just (i, (IMap.size hash) - 1)
        else findCSumHelper target tail (IMap.insert (sum + x) ((IMap.size hash) + 1) hash) (sum + x)

findMaxMinSum :: [Int] -> Int
findMaxMinSum list = maximum list + minimum list

day9 :: IO Int
day9 = do
  numbers <- parseNumList "day9.txt"
  let sums = initN (take lastNum numbers) in
    return $ findNonSum (drop lastNum numbers) (take lastNum numbers) sums

day9_2 :: IO Int
day9_2 = do
  target <- day9
  numbers <- parseNumList "day9.txt"
  case findCSum target numbers of
    Nothing -> return 0
    Just (start, end) -> do
      return $ findMaxMinSum (take (end - start) (drop start numbers))