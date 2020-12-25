{-# LANGUAGE ViewPatterns #-}
module Day24
  ( day24_2,
    day24,
    lineToCube,
    cubeToLine,
    expandBoundingCube,
    walkOverCube,
    increase
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

data CoordsIncrease = IncX | IncY | IncZ | IncStop
  deriving (Show)
type BoundingCube = ((Int, Int, Int), (Int, Int, Int))

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

swap :: Int -> (Int, Int, Int) -> (BoundingCube, (STUArray s Int Int)) -> [Direction] -> ST s (BoundingCube, (STUArray s Int Int))
swap size coords (bcube, arr) [] = do
  let x = cubeToLine size coords
  val <- readArray arr x
  writeArray arr x ((val + 1) `mod` 2)
  return (expandBoundingCube bcube coords,  arr)
swap size coords arr (dir : tail) = swap size (cubeHexMove coords dir) arr tail

countBlack :: Int -> UArray Int Int -> Int
countBlack iteration arr = foldl (\acc el -> if isCurrentlyActive iteration el then acc + 1 else acc) 0 (elems arr)

lineToCube :: Int -> Int -> (Int, Int, Int)
lineToCube size index = ((index `mod` size), (index `div` size) `mod` size, index `div` (size * size))

updateTile :: Int -> Int -> STUArray s Int Int -> Int -> BoundingCube -> (Int, Int, Int) -> ST s (BoundingCube, STUArray s Int Int)
updateTile iteration size arr index bcube coords = do
  curVal <- readArray arr index
  (min, max) <- getBounds arr
  let ncoords = map (cubeHexMove coords) [E, W, SE, SW, NW, NE]
  vals <- mapM (readArray arr) $ filter (\el -> el >= min && el <= max) $ map (cubeToLine size) ncoords
  let blackUp = sum (map (fromEnum . (isPreviouslyActive iteration))  vals)
  let isBlack = (if isCurrentlyActive iteration curVal then blackUp == 1 || blackUp == 2 else blackUp == 2)
  let nwCube = if isBlack then expandBoundingCube bcube coords else bcube
  writeArray arr index (getCurrentActivation iteration curVal isBlack)
  return (nwCube, arr)

expandBoundingCube :: BoundingCube -> (Int, Int, Int) -> BoundingCube
expandBoundingCube (min, max) coords = ((expand (>) (subtract 1) min coords), ((expand (<) (+ 1) max coords)))
  where
    expand fn op (x0, y0, z0) (x, y, z) =
      let nwx = if fn x0 (op x) then (op x) else x0
          nwy = if fn y0 (op y) then (op y) else y0
          nwz = if fn z0 (op z) then (op z) else z0 in
        (nwx, nwy, nwz)

walkOverCube :: Int -> BoundingCube -> BoundingCube -> (Int -> BoundingCube -> (Int, Int, Int) -> ST s (BoundingCube, STUArray s Int Int)) -> ST s (BoundingCube, STUArray s Int Int)
walkOverCube size (min, max) nwcube fn = walkOverCubeHelper (min, max) nwcube fn (cubeToLine size min) min
  where
    walkOverCubeHelper :: BoundingCube -> BoundingCube -> (Int -> BoundingCube -> (Int, Int, Int) -> ST s (BoundingCube, STUArray s Int Int)) -> Int -> (Int, Int, Int) -> ST s (BoundingCube, STUArray s Int Int)
    walkOverCubeHelper bounds@((x0, y0, z0), max) nwcube fn index coords@(x, y, z) = case increase bounds coords of
      (nwcoords, IncX) -> (fn index nwcube coords) >>= (\(nwcube, arr) -> walkOverCubeHelper bounds nwcube fn (index + 1) nwcoords)
      (nwcoords, IncY) -> (fn index nwcube coords) >>= (\(nwcube, arr) -> walkOverCubeHelper bounds nwcube fn (index + size - (x - x0)) nwcoords)
      (nwcoords, IncZ) -> (fn index nwcube coords) >>= (\(nwcube, arr) -> walkOverCubeHelper bounds nwcube fn (index + size * size - (y - y0) * size - (x - x0)) nwcoords)
      (nwcoords, IncStop) -> (fn index nwcube coords)

increase ((x0, y0, z0), (x1, y1, z1)) (x, y, z) =
  if x + 1 <= x1
    then ((x + 1, y, z), IncX)
    else if y + 1 <= y1
      then ((x0, y + 1, z), IncY)
      else if z + 1 <= z1
        then ((x0, y0, z + 1), IncZ)
        else ((x, y, z), IncStop)

simulate :: Int -> Int -> Int -> (BoundingCube, STUArray s Int Int) -> ST s (STUArray s Int Int)
simulate 0 iteration size (bcube, arr) = return arr
simulate days iteration size (bcube, arr) = do
  (nwbcube, nwarr) <- walkOverCube size bcube bcube (updateTile iteration size arr)
  simulate (days - 1) (iteration + 1) size  (nwbcube, nwarr)

day24 :: IO Int
day24 = do
  dirs <- parseInput "day24.txt"
  return $ countBlack 0 $ runSTUArray $ (createCube maxSize) >>= (\arr -> fmap snd $ foldM (swap maxSize initHex) ((initHex, initHex), arr) dirs)

day24_2 :: IO Int
day24_2 = do
  let iterations = 100
  dirs <- parseInput "day24.txt"
  return $ countBlack iterations $ runSTUArray $ ((createCube maxSize) >>= (\arr -> (foldM (swap maxSize initHex) ((initHex, initHex), arr) dirs) >>= simulate iterations 0 maxSize))