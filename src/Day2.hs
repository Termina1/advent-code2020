module Day2
  ( day2,
    day2_2
  ) where

import Data.List.Split

data PasswordEntry = Entry (Int, Int) Char String | Err
  deriving (Show)

parsePolicies :: String -> IO [PasswordEntry]
parsePolicies filename = do 
  contents <- readFile filename
  return $ map parseEntry (filter (\n -> (length n) > 0) (splitOn "\n" contents))
  where
    parseEntry :: String -> PasswordEntry
    parseEntry str = 
      case splitOn ": " str of
        (policy : pass : []) -> 
          case splitOn " " policy of
            (range : char : []) ->
              case splitOn "-" range of
                (min : max : []) -> Entry ((read min), (read max)) (head char) pass
                _ -> Err
            _ -> Err
        _ -> Err

isValid1 :: PasswordEntry -> Bool
isValid1 (Entry (min, max) char pass) = let occurences = (foldl (\acc pchar -> if pchar == char then acc + 1 else acc) 0 pass) in
  occurences >= min && occurences <= max

isValid2 :: PasswordEntry -> Bool
isValid2 (Entry (min, max) char pass) = 
  case ((pass !! (min - 1)) == char, (pass !! (max - 1)) == char) of
    (True, False) -> True
    (False, True) -> True
    _ -> False

countValidPasswords :: [PasswordEntry] -> (PasswordEntry -> Bool) -> Int
countValidPasswords passwords isValid = foldl (\acc pass -> if isValid pass then acc + 1 else acc)  0 passwords

day2 :: IO Int
day2 = do 
  passwords <- parsePolicies "day2.txt"
  return $ countValidPasswords passwords isValid1

day2_2 :: IO Int
day2_2 = do
  passwords <- parsePolicies "day2_2.txt"
  return $ countValidPasswords passwords isValid2
  