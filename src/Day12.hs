module Day12
  ( day12,
    day12_2
  ) where

import Data.List.Split
import Control.Monad

data Direction = North Int | West Int | East Int | South Int | RightR Int | LeftR Int | Forward Int

type Rotation = Int
type Coordinates = (Int, Int)
type Position = (Rotation, Coordinates)

rot2dir :: Rotation -> Int -> Direction
rot2dir rot a = case (rot `mod` 360) of
  90 -> South a
  180 -> West a
  270 -> North a
  _ -> East a

move :: Position -> Direction -> Position
move pos@(rot, (north, east)) dir = case dir of
  North a -> (rot, (north + a, east))
  West a -> (rot, (north, east - a))
  East a -> (rot, (north, east + a))
  South a -> (rot, (north - a, east))
  RightR a -> (rot + a, (north, east))
  LeftR a -> (rot - a, (north, east))
  Forward a -> move pos (rot2dir rot a)

rotate :: Int -> Coordinates -> Coordinates
rotate a (y, x) =  (y', x')
  where
    radians :: Int -> Float
    radians d = (fromIntegral d) * (pi/180)
    x' = (x * (round $ cos (radians a))) + (y * (round $ sin (radians a)))
    y' = (-x * (round $ sin (radians a))) + (y * (round $ cos (radians a)))

moveW :: (Coordinates, Coordinates) -> Direction -> (Coordinates, Coordinates)
moveW ((northW, eastW), (north, east)) dir = case dir of
  North a -> ((northW + a, eastW), (north, east))
  West a -> ((northW , eastW - a), (north, east))
  East a -> ((northW , eastW + a), (north, east))
  South a -> ((northW - a, eastW), (north, east))
  RightR a -> (rotate a (northW, eastW), (north, east))
  LeftR a -> (rotate (negate a) (northW, eastW), (north, east))
  Forward a ->((northW, eastW), foldl (\(north, east) _ -> (north + northW, east + eastW)) (north, east) [1..a])

parseFile :: String -> IO [Direction]
parseFile filename = do
  content <- readFile filename
  mapM parseLine (filter (not . null) (splitOn "\n" content))
  where
    parseLine [] = ioError $ userError "Empty line"
    parseLine (x : nums) = let num = read nums in
      return (case x of
        'L' -> LeftR num
        'N' -> North num
        'W' -> West num
        'E' -> East num
        'S' -> South num
        'R' -> RightR num
        _ -> Forward num)

day12 :: IO Int
day12 = do
  directions <- parseFile "day12.txt"
  let (rot, (north, south)) = foldl move (0, (0, 0)) directions in
    return $ (abs north) + (abs south)

day12_2 :: IO Int
day12_2 = do
  directions <- parseFile "day12.txt"
  let (_, (north, south)) = foldl moveW ((1, 10), (0, 0)) directions in
    return $ (abs north) + (abs south)