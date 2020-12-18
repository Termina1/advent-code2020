module Day18
  ( day18,
    day18_2,
  ) where

import Text.Parsec
import Text.Parsec.String

data Operator = Plus | Mult
  deriving (Show, Eq)

data Expression =
  OperatorE Operator Expression Expression |
  ParE Expression |
  NumE Int
  deriving (Show, Eq)


parseParExp :: Parser Expression
parseParExp = do
  char '('
  expr <- parseExpression
  char ')'
  return (ParE expr)

parseNumberExp :: Parser Expression
parseNumberExp = do
  nums <- many1 digit
  return $ NumE $ read nums

parseOperator :: Parser Operator
parseOperator = do
  op <- (char '+') <|> (char '*')
  case op of
    '+' -> return Plus
    '*' -> return Mult

parseTerm :: Parser Expression
parseTerm = parseParExp <|> parseNumberExp

parseOperatorExp :: Expression -> Parser Expression
parseOperatorExp expr = do
  char ' '
  operator <- parseOperator
  char ' '
  expr2 <- parseTerm
  let curExpr = (OperatorE operator expr expr2)
  option curExpr (parseOperatorExp curExpr)


parseExpression :: Parser Expression
parseExpression = do
  expr1 <- parseTerm
  option expr1 $ parseOperatorExp expr1


parseHomework :: String -> IO [Expression]
parseHomework filename = do
  result <- parseFromFile (sepBy1 parseExpression newline) filename
  case result of
    Left err -> ioError $ userError ("Parse error: " ++ (show err))
    Right exprs -> return exprs

evaluate :: Expression -> Int
evaluate (NumE num) = num
evaluate (ParE expr) = evaluate expr
evaluate (OperatorE op expr1 expr2) = case op of
  Plus -> (evaluate expr1) + (evaluate expr2)
  Mult -> (evaluate expr1) * (evaluate expr2)

precedence:: Expression -> Expression
precedence (OperatorE Plus (OperatorE Mult expr1' expr2') expr2) = OperatorE Mult expr1' (precedence (OperatorE Plus expr2' expr2))
precedence (OperatorE Plus expr1 (OperatorE Mult expr1' expr2')) = OperatorE Mult expr2' (precedence (OperatorE Plus expr1' expr1))
precedence (OperatorE op expr1 expr2) = OperatorE op (precedence expr1) (precedence expr2)
precedence (ParE expr)= ParE $ precedence expr
precedence expr = expr

precedenceEval :: Expression -> Expression
precedenceEval expr = let nwexpr = precedence expr in
  if expr == nwexpr then nwexpr else precedenceEval nwexpr

day18 :: IO Int
day18 = do
  hw <- parseHomework "day18.txt"
  return $ sum $ map evaluate hw


day18_2 :: IO Int
day18_2 = do
  hw <- parseHomework "day18.txt"
  return $ sum $ map (evaluate . precedenceEval) hw