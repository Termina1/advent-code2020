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
import Data.Int
import Data.Bits
import Data.Maybe
import qualified Data.Matrix as M
import Data.Sort
import Data.List.Split

type Tile = [[Int]]
type EdgeMap = HM.HashMap Int64 (HS.HashSet Int)
type TileMap = HM.HashMap Int Tile
type TileMatrix = HM.HashMap (Int, Int) Tile
type Picutre = M.Matrix Int

data Side = STop | SBottom | SLeft | SRight
  deriving (Show)

parseTileString :: String -> Tile
parseTileString str = map parseTileStringLine (splitOn "\n" str)
  where
    parseTileStringLine :: String -> [Int]
    parseTileStringLine str = map (\el -> if el == '#' then 1 else 0) str

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

flipHorizontal :: Tile -> Tile
flipHorizontal tile = map reverse tile

flipVertical tile = reverse tile

-- edges in order: top, right, bottom, left
tileToEdges :: Tile -> [(Int, Int64)]
tileToEdges tile = reverse $ fst $ foldl (\(acc, tile) _ -> (edgeToNum (head tile) : acc, rotatel tile)) ([], tile) [0..3]

sideToPosition :: Side -> Int
sideToPosition STop = 0
sideToPosition SRight = 1
sideToPosition SBottom = 2
sideToPosition SLeft = 3

rotateSide180 :: Side -> Side
rotateSide180 STop = SBottom
rotateSide180 SRight = SLeft
rotateSide180 SBottom = STop
rotateSide180 SLeft = SRight


edgeToNum :: [Int] -> (Int, Int64)
edgeToNum edge = let (one, two) = ((foldl (\acc x -> acc * 2 + x) 0 edge), (foldl (\acc x -> acc * 2 + x) 0 (reverse edge))) in
  if one > two
    then (one, (fromIntegral (two `shiftL` 32)) .|. (fromIntegral one))
    else (one, (fromIntegral (one `shiftL` 32)) .|. (fromIntegral two))

tilesToEdgeMap :: [(Int, Tile)] -> EdgeMap
tilesToEdgeMap tiles = foldl insertTile HM.empty tiles
  where
    insertTile :: EdgeMap -> (Int, Tile) -> EdgeMap
    insertTile mp (tileId, tile) = foldl (\mp (_, edgeHash) ->
      case HM.lookup edgeHash mp of
        Nothing -> HM.insert edgeHash (HS.singleton tileId) mp
        Just ids -> HM.insert edgeHash (HS.insert tileId ids) mp) mp (tileToEdges tile)

findCorners :: EdgeMap -> [(Int, Tile)] -> [Int]
findCorners mp tiles = map fst $ filter (isCornerTile mp) tiles
  where
    isCornerTile :: EdgeMap -> (Int, Tile) -> Bool
    isCornerTile mp (tileId, tile) = (length $ filter isUnmatchedEdge (tileToEdges tile)) >= 2

    isUnmatchedEdge :: (Int, Int64) -> Bool
    isUnmatchedEdge (_, edgeHash) = case HM.lookup edgeHash mp of
      Nothing ->True
      Just k -> (HS.size k) == 1

rotateTimes :: Int -> Tile -> Tile
rotateTimes 0 tile = tile
rotateTimes n tile = rotatel (rotateTimes (n - 1) tile)

tileRotate :: Int64 -> Int -> Side -> Tile -> Tile
tileRotate edgeHash edge side tile = let (foundEdge, diff) = countDiffBetweenSides edgeHash side (tileToEdges tile) in
  flipIfNeeded foundEdge edge side (rotateTimes diff tile)
  where
    countDiffBetweenSides :: Int64 -> Side -> [(Int, Int64)] -> (Int, Int)
    countDiffBetweenSides edgeHash side edges = case elemIndex edgeHash (map snd edges) of
      Nothing -> (0, 0)
      Just pos -> (fst (edges !! pos), (pos - (sideToPosition side) + 4) `mod` 4)

    flipIfNeeded :: Int -> Int -> Side -> Tile -> Tile
    flipIfNeeded foundEdge edge STop tile = if edge == foundEdge then flipHorizontal tile else tile
    flipIfNeeded foundEdge edge SBottom tile = if edge == foundEdge then flipHorizontal tile else tile
    flipIfNeeded foundEdge edge side tile = if edge == foundEdge then flipVertical tile else tile

checkSet :: Int -> HS.HashSet Int -> Maybe Bool
checkSet member hs = if HS.member member hs then Nothing else Just False

placeTiles :: [(Int, Tile)] -> EdgeMap -> TileMatrix
placeTiles tiles mp =
  let corner = head $ findCorners mp tiles
      tilesMap = HM.fromList tiles in
        case HM.lookup corner tilesMap of
          Nothing -> HM.empty
          Just tile -> let (tileMatrix, _, _, _) = placeTilesHelper tilesMap mp corner (HM.singleton (0, 0) tile) (0, 0) (HS.singleton corner) in
            tileMatrix
  where
    placeTilesHelper :: TileMap -> EdgeMap -> Int -> TileMatrix -> (Int, Int) -> HS.HashSet Int -> (TileMatrix, EdgeMap, HS.HashSet Int, TileMap)
    placeTilesHelper tileMap edgeMap tileId tileMatrix coords found = case HM.lookup tileId tileMap of
      Nothing -> (tileMatrix, edgeMap, found, tileMap)
      Just tile -> foldl (placeTile tileId coords) (tileMatrix, edgeMap, found, tileMap) (zip [STop, SRight, SBottom, SLeft] (tileToEdges tile))

    placeTile :: Int -> (Int, Int) -> (TileMatrix, EdgeMap, HS.HashSet Int, TileMap) -> (Side, (Int, Int64)) -> (TileMatrix, EdgeMap, HS.HashSet Int, TileMap)
    placeTile tileId coords (tileMatrix, edgeMap, found, tileMap) (side, (edge, edgeHash)) = fromMaybe (tileMatrix, edgeMap, found, tileMap) (do
      tileIds <- HM.lookup edgeHash edgeMap
      nextTileId <- find (/= tileId) (HS.toList tileIds)
      isFound <- checkSet nextTileId found
      nextTile <- HM.lookup nextTileId tileMap
      let (nwcoords, nwTileMatrix, rotatedTile) = updateTileMatrix coords edgeHash edge tileMatrix side nextTile
      return $ placeTilesHelper (HM.insert nextTileId rotatedTile tileMap) (HM.delete edgeHash edgeMap) nextTileId nwTileMatrix nwcoords (HS.insert tileId found))

    updateTileMatrix :: (Int, Int) -> Int64 -> Int -> TileMatrix -> Side -> Tile -> ((Int, Int), TileMatrix, Tile)
    updateTileMatrix (x, y) edgeHash edge pic side tile =
      let rotatedTile = tileRotate edgeHash edge (rotateSide180 side) tile in
        case side of
          STop -> ((x, y - 1), HM.insert (x, y - 1) rotatedTile pic, rotatedTile)
          SRight -> ((x + 1, y), HM.insert (x + 1, y) rotatedTile pic, rotatedTile)
          SBottom -> ((x, y + 1), HM.insert (x, y + 1) rotatedTile pic, rotatedTile)
          SLeft -> ((x - 1, y), HM.insert (x - 1, y) rotatedTile pic, rotatedTile)


firstLast :: [a] -> [a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

placedTilesToMatrix :: Int -> (Int, Int, Int, Int) -> TileMatrix -> Picutre
placedTilesToMatrix tileSize (x0, x1, y0, y1) tiles = foldl (\acc x -> foldl (\acc y -> (placeTile x y acc)) acc [y0..y1]) initMatrix [x0..x1]
  where
    initMatrix = M.zero ((x1 - x0 + 1) * (tileSize - 2)) ((y1 - y0 + 1) * (tileSize - 2))
    placeTile :: Int -> Int -> Picutre -> Picutre
    placeTile x y acc = case HM.lookup (x, y) tiles of
      Nothing -> acc
      Just tile -> writeTile (removeEdges tile) ((x - x0) * (tileSize - 2) + 1) ((y - y0) * (tileSize - 2) + 1) acc

    removeEdges :: Tile -> Tile
    removeEdges tile = firstLast $ (map firstLast tile)

writeTile :: Tile -> Int -> Int -> Picutre -> Picutre
writeTile tile x y pic = foldl (\pic (y', line) -> foldl (\pic (x', el) -> M.setElem el (x + x', y + y') pic) pic (zip [0..] line)) pic (zip [0..] tile)

pprintTile :: Tile -> String
pprintTile tile = intercalate "\n" $ map (\el -> map (\el -> if el == 1 then '#' else '.') el) tile

reconstructImage :: [(Int, Tile)] -> EdgeMap -> Picutre
reconstructImage tiles mp =
  let tileMatrix = placeTiles tiles mp
      bounds = getBounds tileMatrix in
    placedTilesToMatrix (length $ snd $ head tiles) bounds tileMatrix
  where
    getBounds :: TileMatrix -> (Int, Int, Int, Int)
    getBounds matrix = foldl (\(x0, x1, y0, y1) (x, y) -> ((min x0 x), (max x1 x), (min y0 y), (max y1 y))) (10000, -10000, 10000, -10000) (HM.keysSet matrix)


seaMonster :: Picutre
seaMonster = M.fromLists $ parseTileString $ "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "

rotatePic :: Picutre -> Picutre
rotatePic = M.fromLists . rotatel . M.toLists

flipPic :: Picutre -> Picutre
flipPic = M.fromLists . flipHorizontal . M.toLists

showPicture :: Picutre -> String
showPicture pic = pprintTile $ M.toLists pic

findSeaMonster :: Picutre -> Picutre -> Int
findSeaMonster monster pic = (findSeaMonsterHelper monster pic 4) + (findSeaMonsterHelper monster (flipPic pic) 4)
  where
    findSeaMonsterHelper :: Picutre -> Picutre -> Int -> Int
    findSeaMonsterHelper monster pic 0 = 0
    findSeaMonsterHelper monster pic n =
      let (found, density) = pictureSearch monster pic in
        if found
          then density
          else findSeaMonsterHelper monster (rotatePic pic) (n - 1)

pictureSum :: Picutre -> Int
pictureSum = sum . M.toList

pictureSearch :: Picutre -> Picutre -> (Bool, Int)
pictureSearch monster pic = let found = pictureSearchHelper (pictureSum monster) monster pic (((M.nrows pic) - (M.nrows monster)) * ((M.ncols pic) - (M.ncols monster))) 0 in
  (found > 0, if found > 0 then ((pictureSum pic) - (pictureSum monster) * found) else 0)
  where
    pictureSearchHelper :: Int -> Picutre -> Picutre -> Int -> Int -> Int
    pictureSearchHelper targetSum monster pic 2 found = found
    pictureSearchHelper targetSum monster pic iteration found =
      let (sRow, sCol) = (iteration `div` ((M.ncols pic) - (M.ncols monster) + 1) + 1, iteration `mod` ((M.ncols pic) - (M.ncols monster) + 1) + 1)
          window = M.submatrix sRow (sRow + (M.nrows monster) - 1) sCol (sCol + (M.ncols monster) - 1) pic
          result = pictureSum $ M.elementwise (*) window monster in
          if result == targetSum
              then pictureSearchHelper targetSum monster pic (iteration - 1) (found + 1)
              else pictureSearchHelper targetSum monster pic (iteration - 1) found

day20 :: IO Int
day20 = do
  tiles <- parseInput "day20.txt"
  let edgesMap = tilesToEdgeMap tiles
  return $ product $ findCorners edgesMap tiles


day20_2 :: IO Int
day20_2 = do
  tiles <- parseInput "day20.txt"
  let edgesMap = tilesToEdgeMap tiles
  let pic = reconstructImage tiles edgesMap
  return $ findSeaMonster seaMonster pic