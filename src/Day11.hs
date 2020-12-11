module Day11
  ( day11,
    day11_2
  ) where

import Prelude hiding (Left, Right)

import Data.List.Split
import Data.Array
import Data.List
import LibLib

data Seat = Occupied | Floor | Empty
  deriving (Show, Eq)

data Direction = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight
  deriving (Show)

type SeatRow = Array Int Seat
type SeatScheme = Array Int SeatRow

allDirections :: [Direction]
allDirections = [Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight]

pprint :: SeatScheme -> String
pprint scheme = intercalate "\n" (map pprintRow (elems scheme))
  where
    pprintRow :: SeatRow -> String
    pprintRow row = concat $ map pprintSeat (elems row)

    pprintSeat :: Seat -> String
    pprintSeat Occupied = "#"
    pprintSeat Floor = "."
    pprintSeat Empty = "L"

readSeatRow :: String -> SeatRow
readSeatRow row = let indexedList = map (\(index, el) -> (index, (charToSeat el))) (zip [0..] row) in
  array (0, (length indexedList) - 1) indexedList
  where
    charToSeat '#' = Occupied
    charToSeat '.' = Floor
    charToSeat _ = Empty

readSeatsScheme :: String -> IO SeatScheme
readSeatsScheme filename = do
  content <- readFile filename
  let indexedList = map (\(index, el) -> (index, readSeatRow el)) (zip [0..] (splitOn "\n" content)) in
    return $ array (0, (length indexedList) - 1) indexedList

indexGenerator :: (Int, Int) -> Direction -> (Int, Int)
indexGenerator (i, j) Up = (i + 1, j)
indexGenerator (i, j) Down = (i - 1, j)
indexGenerator (i, j) Left  = (i, j - 1)
indexGenerator (i, j) Right = (i, j + 1)
indexGenerator (i, j) UpLeft = (i + 1, j - 1)
indexGenerator (i, j) UpRight = (i + 1, j + 1)
indexGenerator (i, j) DownLeft = (i - 1, j - 1)
indexGenerator (i, j) DownRight = (i -  1, j + 1)

safeLookup :: (Ix i) => i -> Array i e -> Maybe e
safeLookup i arr = let (min, max) = bounds arr in
  if i >= min && i <= max then Just (arr ! i) else Nothing

countVisibleOccupance :: SeatScheme -> Int -> Int -> Int
countVisibleOccupance scheme i j = sum $ map (findFirstVisibleSeat scheme (i, j)) allDirections
  where
    findFirstVisibleSeat :: SeatScheme -> (Int, Int) -> Direction -> Int
    findFirstVisibleSeat scheme index direction = let (i, j) = indexGenerator index direction in
      case safeLookup i scheme of
        Nothing -> 0
        Just row -> case safeLookup j row of
          Nothing -> 0
          Just Floor -> findFirstVisibleSeat scheme (i, j) direction
          Just Occupied -> 1
          Just Empty -> 0

countAdjacentOccupancy :: SeatScheme -> Int -> Int -> Int
countAdjacentOccupancy scheme i j = foldl (occupant scheme) 0 (map (indexGenerator (i, j)) allDirections)
  where
    occupant :: SeatScheme -> Int -> (Int, Int) -> Int
    occupant scheme acc (i, j) = case safeLookup i scheme of
      Nothing -> acc
      Just row -> case safeLookup j row of
        Nothing -> acc
        (Just Occupied) -> acc + 1
        (Just _) -> acc

lifeIteration :: Int -> (SeatScheme -> Int -> Int -> Int) -> SeatScheme -> SeatScheme
lifeIteration maxOccupant countOccupancy scheme = array (bounds scheme) $ map (outterCycle scheme) (assocs scheme)
  where
    outterCycle :: SeatScheme -> (Int, SeatRow) -> (Int, SeatRow)
    outterCycle scheme (i, row) = (i, array (bounds row) $ map (innerCycle scheme i) (assocs row))

    innerCycle :: SeatScheme -> Int -> (Int, Seat) -> (Int, Seat)
    innerCycle scheme i (j, seat) = (j, updateSeat scheme i j seat)

    updateSeat :: SeatScheme -> Int -> Int -> Seat -> Seat
    updateSeat scheme i j Floor = Floor
    updateSeat scheme i j Empty = if countOccupancy scheme i j == 0 then Occupied else Empty
    updateSeat scheme i j Occupied = let occup = countOccupancy scheme i j in
       if occup >= maxOccupant then Empty else Occupied

countOccupiedSeats :: SeatScheme -> Int
countOccupiedSeats scheme = foldr (\el acc -> foldr occupant acc (elems el)) 0 (elems scheme)
  where
    occupant Occupied acc = acc + 1
    occupant _ acc = acc

life :: (SeatScheme -> Int -> Int -> Int) -> Int -> SeatScheme -> SeatScheme
life countOccupancy maxOccupant scheme = loopLife scheme (lifeIteration maxOccupant countOccupancy scheme)
  where
    loopLife oldScheme newScheme = if oldScheme == newScheme
      then newScheme
      else loopLife newScheme (lifeIteration maxOccupant countOccupancy newScheme)

day11 :: IO Int
day11 = do
  scheme <- readSeatsScheme "day11.txt"
  return $ countOccupiedSeats (life countAdjacentOccupancy 4 scheme)

day11_2 :: IO Int
day11_2 = do
  scheme <- readSeatsScheme "day11.txt"
  return $ countOccupiedSeats (life countVisibleOccupance 5 scheme)

