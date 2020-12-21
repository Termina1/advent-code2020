module Day20
  ( day20,
    day20_2,
    rotatel
  ) where

import Text.Parsec
import Text.Parsec.String
import Data.Array
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Debug.Trace
import Data.Int
import Data.Bits

type Tile = [[Int]]
type EdgeMap = HM.HashMap Int64 (HS.HashSet Int)


parseTileLine :: Parser [Int]
parseTileLine = do
  symbols <- many1 (char '#' <|> char '.')
  return $ map (\el -> if el == '#' then 1 else 0) symbols

parseTile :: Parser (Int, Tile)
parseTile = do
  string "Tile "
  num <- many1 digit
  char ':'
  newline
  tileLines <- sepEndBy1 parseTileLine newline
  return (read num, tileLines)

parseInput :: String -> IO [(Int, Tile)]
parseInput filename = do
  result <- parseFromFile (sepEndBy1 parseTile newline) filename
  case result of
    Left err -> ioError $ userError (show err)
    Right tiles -> return tiles

rotatel :: [[x]] -> [[x]]
rotatel = transpose . map reverse

-- edges in order: top, right, bottom, left
tileToEdges :: Tile -> [Int64]
tileToEdges tile = reverse $ fst $ foldl (\(acc, tile) _ -> (edgeToNum (head tile) : acc, rotatel tile)) ([], tile) [0..3]

edgeToNum :: [Int] -> Int64
edgeToNum edge = let (one, two) = ((foldl (\acc x -> acc * 2 + x) 0 edge), (foldl (\acc x -> acc * 2 + x) 0 (reverse edge))) in
  if one > two
    then (fromIntegral (two `shiftL` 32)) .|. (fromIntegral one)
    else (fromIntegral (one `shiftL` 32)) .|. (fromIntegral two)

tilesToEdgeMap :: [(Int, Tile)] -> EdgeMap
tilesToEdgeMap tiles = foldl insertTile HM.empty tiles
  where
    insertTile :: EdgeMap -> (Int, Tile) -> EdgeMap
    insertTile mp (tileId, tile) = foldl (\mp edge ->
      case HM.lookup edge mp of
        Nothing -> HM.insert edge (HS.singleton tileId) mp
        Just ids -> HM.insert edge (HS.insert tileId ids) mp) mp (tileToEdges tile)

findCorners :: EdgeMap -> [(Int, Tile)] -> [Int]
findCorners mp tiles = map fst $ filter (isCornerTile mp) tiles
  where
    isCornerTile :: EdgeMap -> (Int, Tile) -> Bool
    isCornerTile mp (tileId, tile) = (length $ filter isUnmatchedEdge (tileToEdges tile)) >= 2

    isUnmatchedEdge :: Int64 -> Bool
    isUnmatchedEdge edge = case HM.lookup edge mp of
      Nothing ->True
      Just k -> (HS.size k) == 1

day20 :: IO Int
day20 = do
  tiles <- parseInput "day20.txt"
  let edgesMap = tilesToEdgeMap tiles
  return $ product $ findCorners edgesMap tiles


day20_2 :: IO Int
day20_2 = do
  return 1