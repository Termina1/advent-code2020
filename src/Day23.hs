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
    cups2,
    play,
    answer2,
    peek,
    LinkedList(..)
  ) where

import Data.List
import Debug.Trace
import qualified Data.Sequence as SQ
import Data.Sequence (viewl, viewr, (<|), (|>), ViewR(..), ViewL(..), Seq, (><))
import Data.Foldable
import Data.Maybe
import qualified Data.HashMap.Lazy as HM

class Circle c where
  takeC :: Int -> c Int -> [Int]
  dropC :: Int -> c Int -> c Int
  peek :: c Int -> Int
  skip :: Int -> c Int -> c Int
  insertC :: [Int] -> c Int -> c Int
  size :: c Int -> Int
  rotateTo :: Int -> c Int -> c Int


data LinkedList a = LinkedList a (HM.HashMap a (a, a))
  deriving (Show)

takeCHelper :: Int -> Int -> (HM.HashMap Int (Int, Int)) -> [Int] -> (Int, [Int])
takeCHelper 0 cur hm acc = (cur, reverse acc)
takeCHelper num cur hm acc = case HM.lookup cur hm of
  Just (prev, next) -> takeCHelper (num - 1) next hm (cur : acc)

insertCHelper :: [Int] -> Int -> Int -> Int -> (HM.HashMap Int (Int, Int)) -> (HM.HashMap Int (Int, Int))
insertCHelper [] cur end prev hm = case HM.lookup end hm of
  Just (_, next) -> HM.insert end (cur, next) (HM.insert cur (prev, end) hm)
insertCHelper (hd : tail) cur end prev hm = insertCHelper tail hd end cur (HM.insert cur (prev, hd) hm)

instance Circle LinkedList where
  takeC num (LinkedList cur hm) = snd $ takeCHelper num cur hm []

  dropC num store@(LinkedList cur hm) = fromMaybe store $ do
    let (afterLast, taken) = takeCHelper num cur hm []
    (beforeFirst, _) <- HM.lookup (head taken) hm
    (prev, _) <- HM.lookup beforeFirst hm
    let updated1 = HM.insert beforeFirst (prev, afterLast) hm
    (_, next) <- HM.lookup afterLast hm
    let updated2 = HM.insert afterLast (beforeFirst, next) updated1
    return $ LinkedList afterLast updated2

  peek (LinkedList cur hm) = cur

  skip 0 store = store
  skip num (LinkedList cur hm) = case HM.lookup cur hm of
    Just (prev, next) -> if num < 0
      then skip (num + 1) (LinkedList prev hm)
      else skip (num - 1) (LinkedList next hm)

  insertC lst store@(LinkedList cur hm) = case HM.lookup cur hm of
    Just (prev, next) -> LinkedList cur $ insertCHelper lst cur next prev hm

  size (LinkedList cur hm) = HM.size hm

  rotateTo el (LinkedList cur hm) = LinkedList el hm

instance Circle [] where
  takeC num lst = take num $ cycle lst

  dropC num lst = drop num lst

  peek = head

  skip num lst = take (length lst) $ drop (num `mod` (length lst)) $ cycle lst

  insertC lstin lst = (head lst) : (lstin ++ (tail lst))

  size = length

  rotateTo el lst = case elemIndex el lst of
    Nothing -> lst
    Just k -> skip k lst

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

  insertC lstint (viewl -> EmptyL) = (SQ.fromList lstint)
  insertC lstint (viewl -> hd :< tail) = hd <| ((SQ.fromList lstint) >< tail)

  skip num sq = case compare num 0 of
    EQ -> sq
    GT -> (dropL num sq) >< (takeL num sq)
    LT -> (takeR (-num) sq) >< dropR (-num) sq

  rotateTo val sq = case SQ.elemIndexL val sq of
    Nothing -> sq
    Just k -> skip k sq


skipWhile :: (Circle c) => (Int -> Bool) -> c Int -> c Int
skipWhile fn circle =
  if fn (peek circle)
    then skipWhile fn (skip 1 circle)
    else circle


cupsList :: [Int]
cupsList = [2, 1, 5, 6, 9, 4, 7, 8, 3]

cups = let hd = head cupsList in
  insertC (tail cupsList) $ LinkedList hd (HM.singleton hd (hd, hd))

cups2 =
  let list = cupsList ++ [i | i <- [((maximum cupsList) + 1)..1000000]]
      hd = head list in
    insertC (tail list) $ LinkedList hd (HM.singleton hd (hd, hd))


play :: (Circle c, Show (c Int)) => Int -> Int -> c Int -> c Int
play max 0 cups = cups
play max moves cups =
  let taken = takeC 3 (skip 1 cups)
      nwCircle = dropC 3 (skip 1 cups)
      target = finSearchTarget taken max ((peek cups) - 1) in
    play max (moves - 1) $ skip 1 $ rotateTo (peek cups) $ insertC taken $ rotateTo target nwCircle
  where
    finSearchTarget :: [Int] -> Int -> Int -> Int
    finSearchTarget taken max 0 = case elemIndex max taken of
      Nothing -> max
      Just _ -> finSearchTarget taken (max - 1) 0
    finSearchTarget taken max target = case elemIndex target taken of
      Nothing -> target
      Just _ -> finSearchTarget taken max (target - 1)

answer :: Circle a => a Int -> String
answer cups = concat $ map show $ takeC ((size cups) - 1) $ skip 1 $ rotateTo 1 cups

answer2 :: Circle a => a Int -> Int
answer2 cups = let t = skip 1 $ rotateTo 1 cups in
  (peek t) * (peek $ skip 1 t)

day23 :: IO String
day23 = do
  return $ answer $ play (maximum cupsList) 100 cups

day23_2 :: IO Int
day23_2 = do
  return $ answer2 $ play 1000000 10000000 cups2