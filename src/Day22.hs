{-# LANGUAGE ViewPatterns #-}

module Day22
  ( day22,
    day22_2,
  ) where

import Prelude hiding (take, length)
import Data.List.Split
import Data.Sequence hiding (reverse, zip)
import Data.Foldable (toList)
import qualified Data.HashSet as HS

type Deck = Seq Int
data Player = Player1 Deck | Player2 Deck | AutoWin
  deriving (Show)
type GameStates = HS.HashSet ([Int], [Int])

unpackDeck :: Player -> Deck
unpackDeck (Player1 deck) = deck
unpackDeck (Player2 deck) = deck

parseCards :: String -> IO (Deck, Deck)
parseCards filename = do
  contents <- readFile filename
  let [player1, player2] = splitOn "\n\n" contents
  return (parseDeck player1, parseDeck player2)
  where
    parseDeck :: String -> Deck
    parseDeck deck = fromList $ map read (tail $ splitOn "\n" deck)


play :: Deck -> Deck -> Int
play (viewl -> EmptyL) b = score b
play a (viewl -> EmptyL) = score a
play (viewl -> acard :< acards) (viewl -> bcard :< bcards) =
  if acard > bcard
    then play (acards |> acard |> bcard) bcards
    else play acards (bcards |> bcard |> acard)

store :: Deck -> Deck -> GameStates -> GameStates
store deck1 deck2 states = HS.insert (toList deck1, toList deck2) states

check :: Deck -> Deck -> GameStates -> Bool
check deck1 deck2 states = HS.member (toList deck1, toList deck2) states

playRecursive :: GameStates -> Deck -> Deck -> Player
playRecursive states (viewl -> EmptyL) b = Player2 b
playRecursive states a (viewl -> EmptyL) = Player1 a
playRecursive states deck1@(viewl -> acard :< acards) deck2@(viewl -> bcard :< bcards) =
  let nwstates = store deck1 deck2 states
      won = (if check deck1 deck2 states
                          then AutoWin
                          else if length acards >= acard && length bcards >= bcard
                            then playRecursive HS.empty (take acard acards) (take bcard bcards)
                            else if acard > bcard then Player1 acards else Player2 bcards) in
    case won of
      Player1 _ -> playRecursive nwstates (acards |> acard |> bcard) bcards
      Player2 _ -> playRecursive nwstates acards (bcards |> bcard |> acard)
      AutoWin -> Player1 deck1

score :: Deck -> Int
score sq = sum $ map (\(a, b) -> a * b) (zip [1..] (reverse $ toList sq))

day22 :: IO Int
day22 = do
  (player1, player2) <- parseCards "day22.txt"
  return $ play player1 player2

day22_2 :: IO Int
day22_2 = do
  (player1, player2) <- parseCards "day22.txt"
  return $ score $ unpackDeck $ playRecursive HS.empty player1 player2