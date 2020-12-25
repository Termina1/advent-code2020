module Day25
  ( day25_2,
    day25,
  ) where

import Debug.Trace

pkey1 = 19241437
pkey2 = 17346587
sbn = 7

rm = 20201227

transform :: Int -> Int -> Int
transform loopsize subject = transformHelper loopsize subject 1
  where
    transformHelper 0 subject value = value
    transformHelper loopsize subject value = transformHelper (loopsize - 1) subject ((value * subject) `mod` rm)

bruteforce :: Int -> Int
bruteforce pkey = bruteforceHelper pkey 0 1
  where
    bruteforceHelper pkey loopsize val = let curVal =  (val * sbn) `mod` rm in
      if curVal == pkey
        then loopsize + 1
        else bruteforceHelper pkey (loopsize + 1) curVal

day25 :: IO Int
day25 = do
  return $ transform (bruteforce pkey2) pkey1

day25_2 :: IO Int
day25_2 = do
  return 1
