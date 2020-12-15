module Day15
  ( day15,
    day15_2
  ) where

import Data.Array.IO
import Control.Monad
import qualified Streaming.Prelude as S
import Streaming

type Memory = IOArray Int Int

input :: [Int]
input = [1,20,8,12,0]

startPrev :: Int
startPrev = 14

inputMap :: IO Memory
inputMap = do
  arr <- newArray (0, 30000001) (-1)
  forM (zip [1..] input) (\(i, el) -> writeArray arr el i)
  return arr

next :: Memory -> Int -> Int -> IO Int
next mem prev rnd = do
  el <- readArray mem prev
  case el of
    -1 -> do
      writeArray mem prev (rnd - 1)
      return 0
    k -> do
      writeArray mem prev (rnd - 1)
      return (rnd - k - 1)

gameList :: Memory -> Stream (Of Int) IO ()
gameList mem = do
  S.each (input ++ [startPrev]) >> S.unfoldr (iterateG mem) (startPrev, ((length input) + 2))
  where
    iterateG :: Memory -> (Int, Int) -> IO (Either () (Int, (Int, Int)))
    iterateG mem (prev, rnd) = do
      num <- next mem prev rnd
      return $ Right $ (num, (num, rnd + 1))

day15 :: IO Int
day15 = do
  inputMem <- inputMap
  S.stdoutLn $ S.show $ S.take 1 $ S.drop (2020 - 1) $ gameList inputMem
  return 1

day15_2 :: IO Int
day15_2 = do
  inputMem <- inputMap
  S.stdoutLn $ S.show $ S.take 1 $ S.drop (30000000 - 1) $ gameList inputMem
  return 1