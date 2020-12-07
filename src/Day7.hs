module Day7
  (
    day7,
    day7_2 
  ) where

import Data.List.Split
import Text.Parsec
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Pretty.Simple
import Debug.Trace


type Graph = Map.Map String [(String, Int)]

shinygold :: String
shinygold = "shiny gold"

transpose :: Graph -> Graph
transpose g = Map.foldrWithKey foldOutter Map.empty g
  where
    foldOutter :: String -> [(String, Int)] -> Graph -> Graph
    foldOutter name1 val tg = foldl (foldInner name1) (Map.alter initV name1 tg) val

    foldInner :: String -> Graph -> (String, Int) -> Graph
    foldInner name1 tg (name2, weight) = Map.alter (addEdge (name1, weight)) name2 tg

    addEdge :: (String, Int) -> Maybe [(String, Int)] -> Maybe [(String, Int)]
    addEdge edge Nothing = Just [edge]
    addEdge edge (Just val) = Just (edge : val)

    initV :: Maybe [(String, Int)] -> Maybe [(String, Int)]
    initV Nothing = Just []
    initV v = v

findReachable :: Graph -> String -> Set.Set String
findReachable g name = case Map.lookup name g of
  Nothing -> Set.empty
  Just val -> Set.insert name $ Set.unions (map (findReachable g . fst) val)


day7 :: IO Int
day7 = do 
  text <- readFile "day7.txt"
  case (runParser rulesParser () "test" text) of
    Left err -> return 0
    Right g -> return $ (Set.size $ findReachable (transpose g) shinygold) - 1

countBags :: Graph -> String -> Int
countBags g name = case Map.lookup name g of
  Nothing -> 0
  Just edges -> (sum $ map (\(name, count) -> (countBags g name) * count) edges) + 1


day7_2 :: IO Int
day7_2 = do 
  text <- readFile "day7_2.txt"
  case (runParser rulesParser () "test" text) of
    Left err -> return 0
    Right g -> return $ (countBags g shinygold - 1)

colorParser :: Parsec String () String
colorParser = do
  fst <- many1 letter
  space
  snd <- many1 letter
  space
  (string "bag" >> optional (char 's'))
  return (fst ++ " " ++ snd)

bagParser :: String -> Parsec String () (String, Int)
bagParser str = do
  num <- many1 digit
  space
  color <- colorParser
  return (color, (read num))


ruleParser :: Parsec String () (String, [(String, Int)])
ruleParser = do 
  color <- colorParser
  space
  string "contain"
  space
  colors <- (string "no other bags" >> return []) <|> sepBy1 (bagParser color) (string ", ")
  char '.'
  return $ (color, colors)

rulesParser :: Parsec String () Graph
rulesParser = do
  res <- sepBy1 ruleParser newline
  return $ foldl (\acc (k, v) -> Map.insert k v acc) Map.empty res
