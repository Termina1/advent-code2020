{-# LANGUAGE NamedFieldPuns #-}

module Day16
  ( day16,
    day16_2
  ) where

import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Data.List

type Ticket = [Int]
type FieldsSolution = [(String, Int)]

data Rule = Rule {
  name :: String,
  ranges :: [(Int, Int)]
} deriving (Show, Eq)

rangeParser :: Parser (Int, Int)
rangeParser = do
  start <- many1 digit
  char '-'
  end <- many1 digit
  return (read start, read end)

ruleParser :: Parser Rule
ruleParser = do
  name <- many1 (alphaNum <|> (char ' '))
  string ": "
  ranges <- sepBy1 rangeParser (string " or ")
  return Rule{name, ranges}

ticketParser :: Parser Ticket
ticketParser = do
  nums <- sepBy1 (many1 digit) (char ',')
  return $ map read nums

myTicktParser :: Parser Ticket
myTicktParser = do
  string "your ticket:"
  newline
  ticket <- ticketParser
  newline
  return ticket

nearbyTickets :: Parser [Ticket]
nearbyTickets = do
  string "nearby tickets:"
  newline
  sepBy1 ticketParser newline

inputParser :: Parser ([Rule], Ticket, [Ticket])
inputParser = do
  rules <- sepEndBy1 ruleParser newline
  newline
  myTicket <- myTicktParser
  newline
  tickets <- nearbyTickets
  return (rules, myTicket, tickets)

parseTickets :: String -> IO ([Rule], Ticket, [Ticket])
parseTickets filename = do
  result <- parseFromFile inputParser filename
  case result of
    Left err -> ioError $ userError ("Parse error: " ++ (show err))
    Right dat -> return dat

checkRule :: Int -> Rule -> Bool
checkRule val rule = any (\(start, end) -> start <= val && end >= val) (ranges rule)

ticketErrors :: [Rule] -> Ticket -> [Int]
ticketErrors rules ticket = filter (not . (checkTicketValue rules)) ticket
  where
    checkTicketValue :: [Rule] -> Int -> Bool
    checkTicketValue rules val = any (checkRule val) rules

decypherFieldsNames:: [Ticket] -> [Rule] -> FieldsSolution
decypherFieldsNames tickets rules = solve [] $ decypherFieldsNamesHelper (transpose tickets) rules 0 []
  where
    decypherFieldsNamesHelper :: [[Int]] -> [Rule] -> Int -> [([String], Int)] -> [([String], Int)]
    decypherFieldsNamesHelper [] rules num acc = acc
    decypherFieldsNamesHelper (field : fields) rules num acc =
      let allowedRules = map name (filter (checkRuleAgainstField field) rules) in
      decypherFieldsNamesHelper fields rules (num + 1) ((allowedRules, num) : acc)

    checkRuleAgainstField :: [Int] -> Rule -> Bool
    checkRuleAgainstField field rule = all ((flip checkRule) rule) field

    solve :: FieldsSolution -> [([String], Int)] -> FieldsSolution
    solve acc [] = acc
    solve acc constraints = case find (\(candidates, _) -> length candidates == 1) constraints of
      Just ([name], num) -> solve ((name, num) : acc) (foldl (simplifyConstraints name) [] constraints)
      _ -> acc

    simplifyConstraints :: String -> [([String], Int)] -> ([String], Int) -> [([String], Int)]
    simplifyConstraints sname acc constraint@([name], num) = if name == sname then acc else constraint : acc
    simplifyConstraints sname acc (names, num) = (filter (/= sname) names, num) : acc

decypherTicket :: FieldsSolution -> Ticket -> [(String, Int)]
decypherTicket solution ticket = map (\(name, num) -> (name, ticket !! num)) solution

countDepartureFields :: [(String, Int)] -> Int
countDepartureFields ticket = foldl (\acc (name, val) -> if (take 9 name) == "departure" then acc * val else acc) 1 ticket

day16 :: IO Int
day16 = do
  (rules, _, nearby) <- parseTickets "day16.txt"
  return $ sum $ join $ map (ticketErrors rules) nearby

day16_2 :: IO Int
day16_2 = do
  (rules, myTicket, nearby) <- parseTickets "day16.txt"
  let validTickets = filter (\ticket -> length (ticketErrors rules ticket) == 0) nearby
  let solution = decypherFieldsNames validTickets rules
  let myTicketD = decypherTicket solution myTicket
  return $ countDepartureFields myTicketD