module Day17
  ( day17,
    day17_2,
    getSpaceIndex,
    indexToCoords,
    generateNeighbors
  ) where

import Data.Array.IO
import Data.List.Split
import qualified Streaming.Prelude as S
import Streaming
import Control.Monad
import Control.Exception

type DimSize = Int
type DimCount = Int

-- Defines the space. DimSize is the size of all dimensions, DimCount dimensionality.
data SpaceDef = SpaceDef DimSize DimCount

-- Store for all values. Unboxed for performance.
type Space = IOUArray Int Int

coordNumToLetter :: Int -> String
coordNumToLetter 1 = "x"
coordNumToLetter 2 = "y"
coordNumToLetter 3 = "z"
coordNumToLetter 4 = "t"

-- We don't want to make several copies of arrays. So we encode previous state of cell into its value.
-- The idea is to use 4 numbers on each iteration (each iteration has unique 4 numbers).
-- There is something we call 'base' for each iteration. The formula is base = iteration * 4 + 2. We need to add 2 in the end, because
-- for convinience we start with 0 and 1.
-- So, there are 4 case: if cell was inactive then if now it is active the value is base + 1, if not it is equal base. On the other hand, if
-- it was active and now its inactive the value is base + 3. The last case is when it was active and still is, then the value is base + 3.
-- Thus, having value we can always say two things: whether this cell was already updated and whether it is active still, what state did it have on the previous iteration.
-- Also, all active values have nice property value `mod` 2 == 1, otherwise it is inactive value.
isPreviouslyActive :: Int -> Int -> Bool
isPreviouslyActive iteration value = if value < baseActivation iteration
  then value `mod` 2 == 1
  else if value > (iteration * 4 + 2) + 1 then True else False

baseActivation :: Int -> Int
baseActivation iteration = iteration * 4 + 2

-- There is no iteration arg, so we assue that this function is called only between iterations
isCurrentlyActive :: Int -> Int -> Bool
isCurrentlyActive iteration value = value `mod` 2 == 1

-- Encodes new value for the cell
getCurrentActivation :: Int -> Int -> Bool -> Int
getCurrentActivation iteration value active = let base = baseActivation iteration in
  case isPreviouslyActive iteration value of
    False -> if active then base + 1 else base
    True -> if active then base + 3 else base + 2


-- prints n-dim array as series of 2D arrays
pprint :: Int -> SpaceDef -> Space -> IO ()
pprint iteration def@(SpaceDef size count) space = do
  slice2D def [] (\fixedCoords -> do
    forM_ [0..(size - 1)] (\y -> do
      forM_ [0..(size - 1)] (\x -> do
        res <- readArray space $ getSpaceIndex def ([x, y] ++ fixedCoords)
        if isCurrentlyActive iteration res then putStr "#" else putStr ".")
      putStrLn ""))
  putStrLn ""
  where
    slice2D :: SpaceDef -> [Int] -> ([Int] -> IO ()) -> IO ()
    slice2D (SpaceDef size 2) fixed func = func fixed
    slice2D (SpaceDef size count) fixed func = do
      forM_ [0..(size - 1)] (\coord -> do
        putStrLn $ (coordNumToLetter count) ++ ": " ++ (show coord)
        slice2D (SpaceDef size (count - 1)) (coord : fixed) func)

-- Converts n-dim coordinates into 1-dim index
getSpaceIndex :: SpaceDef -> [Int] -> Int
getSpaceIndex def@(SpaceDef dimSize dimCount) coords = sum $ map (\(dim, coord) -> dimSize ^ dim * coord) (zip [0..(dimCount - 1)] (toCoords def coords))

-- Converts n-dim coordinates into 1-dim index, but interprets [0] as a center
getSpaceIndexFromCenter :: Int -> SpaceDef -> [Int] -> Int
getSpaceIndexFromCenter initialSize def@(SpaceDef size _) coords = getSpaceIndex def (map (\coord -> coord + ((size - initialSize) `div` 2)) (toCoords def coords))

-- Converts 1-dim index to n-dim coords, given space definition
indexToCoords :: SpaceDef -> Int -> [Int]
indexToCoords (SpaceDef dimSize dimCount) index = indexToCoordsHelper index (dimCount - 1) []
  where
    indexToCoordsHelper :: Int -> Int -> [Int] -> [Int]
    indexToCoordsHelper index (-1) acc = acc
    indexToCoordsHelper index curDim acc = let step = dimSize ^ curDim in
      indexToCoordsHelper (index `mod` step) (curDim - 1) ((index `div` step) : acc)

positive :: Int -> Int
positive num
  | num < 0 = 0
  | otherwise = num

toCoords :: SpaceDef -> [Int] -> [Int]
toCoords (SpaceDef _ dimCount) coords = coords ++ (replicate (positive $ dimCount - (length coords)) 0)

-- Creates definiton for space. Infers the needed padding given amount of iterations.
defineSpace :: Int -> Int -> String -> SpaceDef
defineSpace maxIterations dimCount input =
  let split = splitOn "\n" input
      rowsNum = (length split) in
    SpaceDef (((2 * maxIterations) + 2) + rowsNum) dimCount

createSpace :: SpaceDef -> IO Space
createSpace def = newArray (boundsFromDef def) 0
  where
    boundsFromDef :: SpaceDef -> (Int, Int)
    boundsFromDef def@(SpaceDef dimSize dimCount) = (getSpaceIndex def [], getSpaceIndex def (replicate dimCount (dimSize - 1)))

parseMatrix :: SpaceDef -> String -> Space -> IO Space
parseMatrix def contents space = do
  forM_ (zip [0..] (splitOn "\n" contents)) (parseLine def space)
  return space
  where
    parseLine :: SpaceDef -> Space -> (Int, String) -> IO ()
    parseLine def space (y, line) = forM_ (zip [0..] line) $ \(x, val) ->
      let v = if val == '#' then 1 else 0
      in writeArray space (getSpaceIndexFromCenter (length line) def [x, y]) v

-- Generates n-dim neighbors through recursion, removes those, that are out of bounds defined by SpaceDef.
generateNeighbors :: SpaceDef -> [Int] -> [[Int]]
generateNeighbors (SpaceDef dimSize dimCount) coords = filter outOfBounds $ generateNeighborsHelper coords []
  where
    outOfBounds :: [Int] -> Bool
    outOfBounds ncoords = (all (\coord -> coord < dimSize && coord >= 0) ncoords) && coords /= ncoords

    generateNeighborsHelper :: [Int] -> [Int] -> [[Int]]
    generateNeighborsHelper [] acc = [acc]
    generateNeighborsHelper (x : coords) acc =
      (generateNeighborsHelper coords (acc ++ [x - 1])) ++
      (generateNeighborsHelper coords (acc ++ [x + 1])) ++
      (generateNeighborsHelper coords (acc ++ [x]))

setCurrentIndex :: Int -> Int -> Int -> Space -> IO ()
setCurrentIndex iteration index activations space = do
  value <- readArray space index
  let isActive = shouldBeActive value
  writeArray space index (getCurrentActivation iteration value isActive)
  where
    shouldBeActive value = case isPreviouslyActive iteration value of
      True -> activations == 3 || activations == 2
      False -> activations == 3

simulateIteration :: SpaceDef -> Space -> Int -> IO ()
simulateIteration def@(SpaceDef dimSize dimCount) space iteration = do
  forM_ [0..(getSpaceIndex def (replicate dimCount (dimSize - 1)) - 1)] (\index -> do
    let coords = indexToCoords def index
    let neighbors = generateNeighbors def coords
    activations <- filterM (\n -> do
      res <- readArray space (getSpaceIndex def n)
      return $ isPreviouslyActive iteration res) neighbors
    setCurrentIndex iteration index (length activations) space)


countActiveCubes :: Int -> SpaceDef -> Space -> IO Int
countActiveCubes iteration def@(SpaceDef dimSize dimCount) space = foldM (\acc index -> do
  val <- readArray space index
  return $ if isCurrentlyActive iteration val then acc + 1 else acc
  ) 0 [0..(getSpaceIndex def (replicate dimCount (dimSize - 1)) - 1)]

maxIterations :: Int
maxIterations = 6

life :: Int -> Int -> IO Int
life dims iterations = do
  contents <- readFile "day17.txt"
  let def = defineSpace iterations dims contents
  space <- createSpace def
  space <- parseMatrix def contents space
  forM_ [1..maxIterations] (simulateIteration def space)
  countActiveCubes maxIterations def space

day17 :: IO Int
day17 = life 3 maxIterations


day17_2 :: IO Int
day17_2 = life 4 maxIterations