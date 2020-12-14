module Day14
  ( day14,
    day14_2
  ) where

import Data.Char (digitToInt)
import Data.List (foldl')
import Text.Parsec
import Text.Parsec.String
import qualified Data.IntMap as IM
import Data.Bits

type OrMask = Int
type AndMask = Int
type Memory = IM.IntMap Int
type FloatingBits = [Int]
data Operation = MemSet (Int, Int) | MaskSet (OrMask, AndMask) String
  deriving (Show)

readBin :: String -> Int
readBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

convertMask :: String -> (Int, Int)
convertMask mask = (readBin (map (\a -> if a == 'X' then '0' else a) mask), readBin (map (\a -> if a == 'X' then '1' else a) mask))

maskOp :: Parser Operation
maskOp = do
  try $ string "mask = "
  mask <- many1 (digit <|> char 'X')
  return $ MaskSet (convertMask mask) mask

memOp :: Parser Operation
memOp = do
  try $ string "mem["
  reg <- many1 digit
  string "] = "
  num <- many1 digit
  return $ MemSet (read reg, read num)

opsParser :: Parser [Operation]
opsParser = sepBy1 (maskOp <|> memOp) newline

parseOperations :: String -> IO [Operation]
parseOperations filename = do
  result <- parseFromFile opsParser filename
  case result of
    Left err -> ioError $ userError ("Parse error: " ++ (show err))
    Right ops -> return ops

runOperations :: [Operation] -> Memory
runOperations ops = runOperationsHelper ops (0, 1) IM.empty
  where
    runOperationsHelper :: [Operation] -> (AndMask, OrMask) -> Memory -> Memory
    runOperationsHelper [] mask mem = mem
    runOperationsHelper (x : tail) mask mem = case x of
      MaskSet (ormask, andmask) _ -> runOperationsHelper tail (ormask, andmask) mem
      MemSet (index, value) -> runOperationsHelper tail mask (IM.insert index (applyMask mask value) mem)

    applyMask :: (OrMask, AndMask) -> Int -> Int
    applyMask (ormask, andmask) val = (val .|. ormask) .&. andmask

fbitsCombinations :: [Int] -> [[Int]]
fbitsCombinations [] = []
fbitsCombinations bits = fbitsCombinationsHelper bits []
  where
    fbitsCombinationsHelper [] acc = [acc]
    fbitsCombinationsHelper (x : tail) chosen = fbitsCombinationsHelper tail (x : chosen) ++ fbitsCombinationsHelper tail chosen

runOperations_v2 :: [Operation] -> Memory
runOperations_v2 ops = runOperationsHelper_v2 ops (0, []) IM.empty
  where
    runOperationsHelper_v2 :: [Operation] -> (OrMask, FloatingBits) -> Memory -> Memory
    runOperationsHelper_v2 [] mask mem = mem
    runOperationsHelper_v2 (x : tail) mask mem = case x of
      MaskSet (ormask, _) mask -> runOperationsHelper_v2 tail (ormask, (maskToFBits mask)) mem
      MemSet (index, value) -> runOperationsHelper_v2 tail mask (applyMaskWithFBits mem mask index value)

    maskToFBits :: String -> FloatingBits
    maskToFBits mask = foldl (\acc (num, el) -> if el == 'X' then num : acc else acc) [] (zip (reverse [0..((length mask) - 1)]) mask)

    init :: OrMask -> [Int] -> OrMask
    init mask fbits = mask .&. (complement $ foldl (\acc el -> acc .|. (shift 1 el)) 0 fbits)

    applyMaskWithFBits :: Memory -> (OrMask, FloatingBits) -> Int -> Int -> Memory
    applyMaskWithFBits mem (ormask, fbits) index value = let indexes = generateIndexesWithFBits (init (ormask .|. index) fbits) fbits in
      foldl (\acc el -> IM.insert el value acc) mem indexes

    generateIndexesWithFBits :: OrMask -> FloatingBits -> [Int]
    generateIndexesWithFBits ormask fbits = let combinations = fbitsCombinations fbits in
      map (applyFBitCombination ormask) combinations

    applyFBitCombination :: OrMask -> [Int] -> OrMask
    applyFBitCombination ormask bits = foldl (\acc el -> (shift 1 el) .|. acc) ormask bits

memorySum :: Memory -> Int
memorySum mem = IM.foldr (\el acc -> el + acc) 0 mem

day14 :: IO Int
day14 = do
  operations <- parseOperations "day14.txt"
  return $ memorySum $ runOperations operations

day14_2 :: IO Int
day14_2 = do
  operations <- parseOperations "day14.txt"
  return $ memorySum $ runOperations_v2 operations