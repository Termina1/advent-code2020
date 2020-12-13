module Day13
  ( day13,
    day13_2
  ) where

import Data.List.Split

data Timetable = Period Int | OutOfService
  deriving (Show)

parseTimeTable :: String -> Timetable
parseTimeTable "x" = OutOfService
parseTimeTable num = Period (read num)

parseFile :: String -> IO (Int, [Timetable])
parseFile filename = do
  content <- readFile filename
  let lines = (splitOn "\n" content) in
    return (read (lines !! 0), map parseTimeTable (splitOn "," (lines !! 1)))

timeTableToConstraints :: [(Int, Timetable)] -> [(Int, Int)]
timeTableToConstraints timetable = foldl helper [] timetable
  where
    helper acc (n, (Period num)) = acc ++ [(num, n)]
    helper acc _= acc

findClosestBus :: Int -> [Timetable] -> (Int, Int)
findClosestBus dep timetable = foldl findClosestBusHelper (maxBound, maxBound) timetable
  where
    findClosestBusHelper :: (Int, Int) -> Timetable -> (Int, Int)
    findClosestBusHelper acc@(busid, closestdep) (Period el) =
      let nwclosdep = (ceiling (((fromIntegral dep)/(fromIntegral el)) :: Float)) * el in
        if nwclosdep < closestdep then (el, nwclosdep) else acc
    findClosestBusHelper acc OutOfService = acc

chineseRemainder :: [(Int, Int)] -> Int
chineseRemainder nums = let prod = foldl (\acc (n, a) -> acc * n) 1 nums in
  let sum = (foldl (chineseRemainderHelper prod) 0 nums) in
    sum `mod` prod
  where
    chineseRemainderHelper :: Int -> Int -> (Int, Int) -> Int
    chineseRemainderHelper prod sum (n, a) = let p = prod `div` n in
      sum + a * (mulInv p n) * p

    mulInv :: Int -> Int -> Int
    mulInv a 1 = 1
    mulInv a b = if a <= 1 then 1 else let x1 = mulInvHelper a b 0 1 in
      if x1 < 0 then x1 + b else x1

    mulInvHelper :: Int -> Int -> Int -> Int -> Int
    mulInvHelper a b x0 x1 = let q = a `div` b in
      let (a', b') = (b, a `mod` b) in
        let (x0', x1') = (x1 - q * x0, x0) in
          if a' > 1 then mulInvHelper a' b' x0' x1' else x1'

day13 :: IO Int
day13 = do
  (departure, timetable) <- parseFile "day13.txt"
  let (busid, closestdep) = findClosestBus departure timetable in
    return $ busid * (closestdep - departure)

day13_2 :: IO Int
day13_2 = do
  (_, timetable) <- parseFile "day13.txt"
  let ftimetable = timeTableToConstraints $ (zip (reverse [0..((length timetable) - 1)]) timetable) in do
    return $ (chineseRemainder ftimetable) - (snd $ head ftimetable)