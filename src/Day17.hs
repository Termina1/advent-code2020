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
data SpaceDef = SpaceDef DimSize DimCount


type Space = IOUArray Int Int

coordNumToLetter :: Int -> String
coordNumToLetter 1 = "x"
coordNumToLetter 2 = "y"
coordNumToLetter 3 = "z"
coordNumToLetter 4 = "t"

baseActivation :: Int -> Int
baseActivation iteration = iteration * 4 + 2

isPreviouslyActive :: Int -> Int -> Bool
isPreviouslyActive iteration value = if value < baseActivation iteration
  then value `mod` 2 == 1
  else if value > (iteration * 4 + 2) + 1 then True else False

isCurrentlyActive :: Int -> Int -> Bool
isCurrentlyActive iteration value = value `mod` 2 == 1

getCurrentActivation :: Int -> Int -> Bool -> Int
getCurrentActivation iteration value active = let base = baseActivation iteration in
  case isPreviouslyActive iteration value of
    False -> if active then base + 1 else base
    True -> if active then base + 3 else base + 2

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


getSpaceIndex :: SpaceDef -> [Int] -> Int
getSpaceIndex def@(SpaceDef dimSize dimCount) coords = sum $ map (\(dim, coord) -> dimSize ^ dim * coord) (zip [0..(dimCount - 1)] (toCoords def coords))

getSpaceIndexFromCenter :: Int -> SpaceDef -> [Int] -> Int
getSpaceIndexFromCenter initialSize def@(SpaceDef size _) coords = getSpaceIndex def (map (\coord -> coord + ((size - initialSize) `div` 2)) (toCoords def coords))

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
    return $ assert (index == (getSpaceIndex def coords))
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