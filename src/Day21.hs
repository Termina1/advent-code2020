module Day21
  ( day21,
    day21_2,
  ) where

import Text.Parsec
import Text.Parsec.String
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Sort


type FoodList = (HM.HashMap String (HS.HashSet String))

foodParser :: Parser ([String], [String])
foodParser = do
  ingrs <- sepEndBy1 (many1 letter) space
  string "(contains "
  allergens <- sepBy1 (many1 letter) (string ", ")
  char ')'
  return (ingrs, allergens)

foldFoodList :: FoodList -> ([String], [String]) -> FoodList
foldFoodList acc (ingrs, allergs) = foldl intersectInredients acc allergs
  where
    intersectInredients :: FoodList -> String -> FoodList
    intersectInredients acc allerg = case HM.lookup allerg acc of
      Nothing -> HM.insert allerg (HS.fromList ingrs) acc
      Just x -> HM.insert allerg (HS.intersection (HS.fromList ingrs) x) acc

parseInputToHash :: String -> IO [([String], [String])]
parseInputToHash filename = do
  result <- parseFromFile (sepBy1 foodParser newline) filename
  case result of
    Left err -> ioError $ userError (show err)
    Right food -> return food

findAllergenIngrs :: FoodList -> [(String, String)]
findAllergenIngrs lst = findAllergenIngrsHelper (HM.toList lst) HS.empty []
  where
    findAllergenIngrsHelper :: [(String, HS.HashSet String)] -> HS.HashSet String -> [(String, String)] -> [(String, String)]
    findAllergenIngrsHelper lst st acc = case find (\(name, ingrs) -> (HS.size (HS.difference ingrs st)) == 1) lst of
      Nothing -> acc
      Just (name, ingrs) -> findAllergenIngrsHelper lst (HS.union ingrs st) ((name, (head $ HS.toList $ HS.difference ingrs st)) : acc)

canonicalList :: FoodList -> String
canonicalList lst = let pairs = findAllergenIngrs lst in
  intercalate "," $ map snd (sortOn fst pairs)

day21 :: IO Int
day21 = do
  food <- parseInputToHash "day21.txt"
  let foodList = foldl foldFoodList HM.empty food
  let possibleAllergens = HS.unions (HM.elems foodList)
  return $ foldl (\count (ingrs, _) -> foldl (\count ingr -> if HS.member ingr possibleAllergens then count else count + 1) count ingrs) 0 food


day21_2 :: IO String
day21_2 = do
  food <- parseInputToHash "day21.txt"
  let foodList = foldl foldFoodList HM.empty food
  return $ canonicalList foodList