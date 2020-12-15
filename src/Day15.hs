module Day15
  ( day15,
    day15_2
  ) where

import qualified Data.IntMap as IM
import Data.List

type Memory = IM.IntMap Int

input :: [Int]
input = [1,20,8,12,0]

startPrev :: Int
startPrev = 14

inputMap :: IM.IntMap Int
inputMap = foldr (\(num, el) acc -> IM.insert el num acc) IM.empty (zip [1..] input)

next :: Memory -> Int -> Int -> (Int, Memory)
next mem prev rnd = case IM.lookup prev mem of
  Nothing -> (0, IM.insert prev (rnd - 1) mem)
  Just k -> (rnd - k - 1, IM.insert prev (rnd - 1) mem)


gameList :: Memory -> [Int]
gameList mem = input ++ [startPrev] ++ (snd $ mapAccumL iterateG (mem, startPrev) [((length input) + 2)..])
  where
    iterateG :: (Memory, Int) -> Int -> ((Memory, Int), Int)
    iterateG (mem, prev) rnd =  let (num, nwmem) = next mem prev rnd in
      ((nwmem, num), num)

day15 :: IO Int
day15 = do
  return $ head $ drop (2020 - 1) (gameList inputMap)

day15_2 :: IO Int
day15_2 = do
  return $ head $ drop (30000000 - 1) (gameList inputMap)