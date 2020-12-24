{-# LANGUAGE ViewPatterns #-}
module Day24
  ( day24_2,
    day24,
    lineToCube,
    cubeToLine
  ) where

import Text.Parsec
import Text.Parsec.String
import Debug.Trace
import Data.Array.ST
import Control.Monad.ST
import Data.Array.Unboxed
import Control.Monad
import Day17

data Direction = E | SE | SW | W | NW | NE
  deriving (Show)

maxSize = 200
initHex = (maxSize `div` 2, maxSize `div` 2, maxSize `div` 2)

toDir :: String -> Direction
toDir "sw" = SW
toDir "se" = SE
toDir "nw" = NW
toDir "ne" = NE
toDir "w" = W
toDir "e" = E

parseDirections :: Parser [Direction]
parseDirections = do
  dirs <- many1 $ choice $ map try [string "se", string "sw", string "nw", string "ne", string "w", string "e"]
  return $ map toDir dirs

parseInput :: String -> IO [[Direction]]
parseInput filename = do
  result <- parseFromFile (sepEndBy1 parseDirections newline) filename
  case result of
    Left err -> ioError $ userError ("Parse error: " ++ (show err))
    Right exprs -> return exprs


cubeHexMove :: (Int, Int, Int) -> Direction -> (Int, Int, Int)
cubeHexMove (x, y, z) E = (x + 1, y - 1, z)
cubeHexMove (x, y, z) SE = (x, y - 1, z + 1)
cubeHexMove (x, y, z) SW = (x - 1, y, z + 1)
cubeHexMove (x, y, z) W = (x - 1, y + 1, z)
cubeHexMove (x, y, z) NW = (x, y + 1, z - 1)
cubeHexMove (x, y, z) NE = (x + 1, y, z - 1)

cubeToLine :: Int -> (Int, Int, Int) -> Int
cubeToLine size (x, y, z) = x + y * size + z * size * size


createCube :: Int -> ST s (STUArray s Int Int)
createCube size = newArray (0, size ^ 3) 0

swap :: Int -> (Int, Int, Int) -> (STUArray s Int Int) -> [Direction] -> ST s (STUArray s Int Int)
swap size coords arr [] = do
  let x = cubeToLine size coords
  val <- readArray arr x
  writeArray arr x ((val + 1) `mod` 2)
  return arr
swap size coords arr (dir : tail) = swap size (cubeHexMove coords dir) arr tail

countBlack :: Int -> UArray Int Int -> Int
countBlack iteration arr = foldl (\acc el -> if isCurrentlyActive iteration el then acc + 1 else acc) 0 (elems arr)

lineToCube :: Int -> Int -> (Int, Int, Int)
lineToCube size index = ((index `mod` size), (index `div` size) `mod` size, index `div` (size * size))

updateTile :: Int -> Int -> STUArray s Int Int -> Int -> ST s (STUArray s Int Int)
updateTile iteration size arr index = do
  let coords = lineToCube size index
  curVal <- readArray arr index
  (min, max) <- getBounds arr
  vals <- mapM (readArray arr) $ filter (\el -> el >= min && el <= max) $ map ((cubeToLine size) . (cubeHexMove coords)) [E, W, SE, SW, NW, NE]
  let blackUp = sum (map (fromEnum . (isPreviouslyActive iteration))  vals)
  let isBlack = (if isCurrentlyActive iteration curVal then blackUp == 1 || blackUp == 2 else blackUp == 2)
  writeArray arr index (getCurrentActivation iteration curVal isBlack)
  return arr


simulate :: Int -> Int -> Int -> STUArray s Int Int -> ST s (STUArray s Int Int)
simulate 0 iteration size arr = return arr
simulate days iteration size arr = do
  (min, max) <- getBounds arr
  forM_ [min..max] (updateTile iteration size arr)
  simulate (days - 1) (iteration + 1) size arr

day24 :: IO Int
day24 = do
  dirs <- parseInput "day24.txt"
  return $ countBlack 0 $ runSTUArray $ (createCube maxSize) >>= \arr -> foldM (swap maxSize initHex) arr  dirs

day24_2 :: IO Int
day24_2 = do
  let iterations = 100
  dirs <- parseInput "day24.txt"
  return $ countBlack iterations $ runSTUArray $ ((createCube maxSize) >>= \arr -> foldM (swap maxSize initHex) arr  dirs) >>= simulate iterations 0 maxSize