{-# LANGUAGE ViewPatterns #-}

module Day23
  ( day23,
    day23_2,
    Circle,
    skip,
    takeC,
    dropC,
    insertC,
    skipWhile,
    cups2
  ) where

import Data.List
import Debug.Trace
import qualified Data.Sequence as SQ
import Data.Sequence (viewl, viewr, (<|), (|>), ViewR(..), ViewL(..), Seq, (><))
import Data.Foldable
import Data.Maybe

class Circle c where
  takeC :: Int -> c a -> [a]
  dropC :: Int -> c a -> c a
  peek :: c a -> a
  skip :: Int -> c a -> c a
  insertC :: [a] -> c a -> c a
  size :: c a -> Int

instance Circle [] where
  takeC num lst = take num $ cycle lst
  dropC num lst = drop num lst
  peek = head
  skip num lst = take (length lst) $ drop (num `mod` (length lst)) $ cycle lst
  insertC lstin lst = (head lst) : (lstin ++ (tail lst))
  size = length

takeL :: Int -> Seq a -> Seq a
takeL 0 (viewl -> hd :< tail) = SQ.empty
takeL num (viewl -> EmptyL) = SQ.empty
takeL num (viewl -> hd :< tail) = hd <| (takeL (num - 1) tail)

dropL :: Int -> Seq a -> Seq a
dropL num (viewl -> EmptyL) = SQ.empty
dropL 1 (viewl -> hd :< tail) = tail
dropL num (viewl -> hd :< tail) = dropL (num - 1) tail

takeR :: Int -> Seq a -> Seq a
takeR num (viewr -> EmptyR) = SQ.empty
takeR 0 (viewr -> tail :> hd) = SQ.empty
takeR num (viewr -> tail :> hd) = (takeR (num - 1) tail) |> hd

dropR :: Int -> Seq a -> Seq a
dropR num (viewr -> EmptyR) = SQ.empty
dropR 1 (viewr -> tail :> hd) = tail
dropR num (viewr -> tail :> hd) = dropR (num - 1) tail

instance Circle Seq where
  takeC n sq = toList $ takeL n sq
  dropC = dropL
  peek (viewl -> hd :< tail) = hd
  size = SQ.length
  insertC lstint (viewl -> hd :< tail) = hd <| ((SQ.fromList lstint) >< tail)
  skip num sq = if num < 0
    then (takeR (-num) sq) >< dropR (-num) sq
    else (dropL num sq) >< (takeL num sq)


skipWhile :: (Circle c) => (a -> Bool) -> c a -> c a
skipWhile fn circle =
  if fn (peek circle)
    then skipWhile fn (skip 1 circle)
    else circle


cups :: [Int]
cups = [2, 1, 5, 6, 9, 4, 7, 8, 3]

cups2 = SQ.fromList $ cups ++ [i | i <- [(maximum cups)..1000000]]

play :: (Circle c, Show (c Int)) => Int -> Int -> c Int -> c Int
play max 0 cups = cups
play max moves cups =
  let taken = takeC 3 (skip 1 cups)
      nwCircle = dropC 3 (skip 1 cups)
      rotation = selectTarget max taken nwCircle ((peek cups) - 1)
      rotationBack = if rotation < 0 then (-rotation) + (length taken) else (-rotation) in
    play max (traceShowId (moves - 1)) (skip rotationBack $ insertC taken $ skip rotation nwCircle)
  where
    selectTarget :: (Circle c) => Int -> [Int] -> c Int -> Int -> Int
    selectTarget max taken circle target = let searchTarget = finSearchTarget taken max target in
      selectTargetHelper searchTarget (skip (-1) circle) (skip 1 circle) 1

    selectTargetHelper :: Circle a => Int -> a Int -> a Int -> Int -> Int
    selectTargetHelper target left right rotations = if peek left == target
      then (-rotations)
      else if peek right == target
        then rotations
        else selectTargetHelper target (skip (-1) left) (skip 1 right) (rotations + 1)

    finSearchTarget :: [Int] -> Int -> Int -> Int
    finSearchTarget taken max 0 = case elemIndex max taken of
      Nothing -> max
      Just _ -> finSearchTarget taken (max - 1) 0
    finSearchTarget taken max target = case elemIndex target taken of
      Nothing -> target
      Just _ -> finSearchTarget taken max (target - 1)

answer :: Circle a => a Int -> String
answer cups = concat $ map show $ takeC ((size cups) - 1) $ skip 1 $ skipWhile (/= 1) cups

answer2 :: Circle a => a Int -> Int
answer2 cups = let t = skip 1 $ skipWhile (/= 1) cups in
  (peek t) * (peek $ skip 1 t)

day23 :: IO String
day23 = do
  return $ answer $ play (maximum cups) 100 cups

day23_2 :: IO Int
day23_2 = do
  return $ answer2 $ play 1000000 10000000 cups2