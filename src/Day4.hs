module Day4
  ( day4,
    validateColor
  ) where

import Data.List.Split
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack, pack)
import Data.List
import Numeric (readHex)
import Data.Char
import LibLib


data Passport = Passport {
  ecl :: String,
  byr :: String,
  iyr :: String,
  eyr :: String,
  hgt :: String,
  hcl :: String,
  pid :: String,
  cid :: String
} deriving (Show)

initPassport :: Passport
initPassport = Passport {
  ecl = "",
  byr = "",
  iyr = "",
  eyr = "",
  hgt = "",
  hcl = "",
  pid = "",
  cid = ""
} 

data Height = Inches Int | Cms Int | Error deriving (Show)


parseHeight :: String -> Height
parseHeight h = let (val, tp) = span isDigit h in case tp of
  "cm" -> Cms (read val)
  "in" -> Inches (read val)
  _ -> Error

validateEColor :: String -> Bool
validateEColor cl = elem cl ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validateBYear :: String -> Bool
validateBYear y = let by = (read y) :: Int in by >= 1920 && by <= 2002

validateIYear :: String -> Bool
validateIYear y = let by = (read y) :: Int in by >= 2010 && by <= 2020

validateEYear :: String -> Bool
validateEYear y = let by = (read y) :: Int in by >= 2020 && by <= 2030

validateHeight :: String -> Bool
validateHeight h = case parseHeight h of
  Inches a -> a >= 59 && a <= 76
  Cms a -> a >= 150 && a <= 193
  _ -> False

validateColor :: String -> Bool
validateColor c = (head c) == '#' && (length c) == 7 && (all isHexDigit (tail c))

validatePid :: String -> Bool
validatePid pid = (length pid == 9) && ((read pid) :: Int) > 0

parsePassport :: [String] -> Passport
parsePassport fields = Prelude.foldl parseField (initPassport) fields
  where 
    parseField :: Passport -> String -> Passport
    parseField pass opt = case splitOn ":" opt of
      "ecl":value:[] -> if (validateEColor value) then pass { ecl = value } else pass
      "byr":value:[] -> if (validateBYear value) then pass { byr = value } else pass
      "iyr":value:[] -> if (validateIYear value) then pass { iyr = value } else pass
      "eyr":value:[] -> if (validateEYear value) then pass { eyr = value } else pass
      "hgt":value:[] -> if (validateHeight value) then pass { hgt = value } else pass
      "hcl":value:[] -> if (validateColor value) then pass { hcl = value } else pass
      "pid":value:[] -> if (validatePid value) then pass { pid = value } else pass
      "cid":value:[] -> pass { cid = value }
      _ -> pass

isValid :: Passport -> Bool
isValid (Passport (a:a1) (b:b1) (c:c1) (d:d2) (e:e2) (f:f2) (g:g2) _) = True
isValid _ = False

isValidLine :: Int -> ByteString -> Int
isValidLine acc string = 
  if isValid $ parsePassport (words $ unpack string)
    then (acc + 1) 
    else acc

day4 :: IO Int
day4 = runConduitRes $ CB.sourceFile "day4_2.txt" 
  .| splitOnSeq (pack "\n\n")
  .| CL.fold isValidLine 0
