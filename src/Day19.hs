module Day19
  ( day19,
    day19_2,
  ) where

import Data.Array
import Data.List
import Data.Char
import Debug.Trace
import Text.Pretty.Simple
import Data.List.Split
import Control.Monad

data Rule = Terminal Char | Expand Int | Seq Rule Rule | Choice Rule Rule | TNumber | Epsilon | TChar
  deriving (Show)

data ParseTree = PSeq ParseTree ParseTree | PNumber Int | PChar Char | PTerminal Char | PEmpty | PRule Int ParseTree
  deriving (Show)

type Grammar = Array Int Rule
type ParseResult = Either String (String, ParseTree)


grammarRules :: Grammar
grammarRules = array (0, 7) [
  (7, Seq (Expand 0) (Choice (Seq (Terminal '\n') (Expand 7)) Epsilon)),
  (0, Seq TNumber (Seq (Terminal ':') (Seq (Terminal ' ') (Expand 1)))),
  (1, Seq (Expand 3) (Expand 2)),
  (2, Choice (Seq (Terminal ' ') (Seq (Terminal '|') (Seq (Terminal ' ') (Expand 1)))) Epsilon),
  (3, Seq (Expand 5) (Expand 4)),
  (4, Choice (Seq (Terminal ' ') (Expand 3)) Epsilon),
  (5, Choice TNumber (Expand 6)),
  (6, Seq (Terminal '"') (Seq TChar (Terminal '"')))]

pprintG :: Grammar -> IO ()
pprintG grammar = forM_ (assocs grammar) pprintGHelper
  where
    pprintGHelper (num, rule) = putStrLn ((show num) ++ ": " ++ (pprintRule rule))

    pprintRule Epsilon = "eps"
    pprintRule TChar = "anychar"
    pprintRule TNumber = "anynumber"
    pprintRule (Choice r1 r2) = (pprintRule r1) ++ " | " ++ (pprintRule r2)
    pprintRule (Seq r1 r2) = (pprintRule r1) ++ " " ++ (pprintRule r2)
    pprintRule (Expand num) = (show num)
    pprintRule (Terminal char) = (show char)

runAutomataWithDebug :: Grammar -> Rule -> String -> ParseResult
runAutomataWithDebug grammar allr str = runAutomata grammar allr str
  --trace ("Running stack " ++ (show allr) ++ ", string left: " ++ (show str)) (runAutomata grammar allr) str

runAutomata :: Grammar -> Rule -> String -> ParseResult
runAutomata grammar (Terminal char) (curChar : str) = if char == curChar
  then Right (str, PTerminal char)
  else Left $ "Expected " ++ (show char) ++ " got " ++ (show curChar)
runAutomata grammar TNumber str = let num = takeWhile isDigit str in
  if length num == 0
    then Left $ "Expected number got " ++ (show $ head str)
    else Right ((drop (length num) str), PNumber (read num))
runAutomata grammar Epsilon str = Right (str, PEmpty)
runAutomata grammar TChar (curChar : str) = if isLetter curChar
  then Right (str, PChar curChar)
  else Left $ "Expected any letter got " ++ (show curChar)
runAutomata grammar (Expand num) str = runAutomataWithDebug grammar (grammar ! num) str >>= \(str, tree) -> return (str, PRule num tree)
runAutomata grammar (Seq rule1 rule2) str =
  runAutomataWithDebug grammar rule1 str >>=
    (\(str, ltree) -> runAutomataWithDebug grammar rule2 str >>=
      (\(str, rtree) -> return $ ( str, PSeq ltree rtree)))
runAutomata grammar (Choice rule1 rule2) str = case runAutomataWithDebug grammar rule1 str of
  Left err -> runAutomataWithDebug grammar rule2 str
  Right result -> Right result
runAutomata grammar rule "" = Left "Unexpected end of input"

runAutomataWithDebug2 :: Grammar -> [Rule] -> String -> [Bool]
runAutomataWithDebug2 grammar allr str = runAutomata2 grammar allr str
  --trace ("Running stack " ++ (show allr) ++ ", string left: " ++ (show str)) (runAutomata2 grammar allr) str

runAutomata2 :: Grammar -> [Rule] -> String -> [Bool]
runAutomata2 grammar ((Terminal char) : tail) (curChar : str) = if char == curChar
  then runAutomataWithDebug2 grammar tail str
  else [False]
runAutomata2 grammar (TNumber : tail) str = let num = takeWhile isDigit str in
  if length num == 0
    then [False]
    else runAutomataWithDebug2 grammar tail (drop (length num) str)
runAutomata2 grammar (Epsilon : tail) str = runAutomataWithDebug2 grammar tail str
runAutomata2 grammar (TChar : tail) (curChar : str) = if isLetter curChar
  then runAutomataWithDebug2 grammar tail str
  else [False]
runAutomata2 grammar ((Expand num) : tail) str = runAutomataWithDebug2 grammar ((grammar ! num) : tail) str
runAutomata2 grammar ((Seq rule1 rule2) : tail) str = runAutomataWithDebug2 grammar (rule1 : rule2 : tail) str
runAutomata2 grammar ((Choice rule1 rule2) : tail) str = (runAutomataWithDebug2 grammar (rule1 : tail) str) ++ (runAutomataWithDebug2 grammar (rule2 : tail) str)
runAutomata2 grammar [] "" = [True]
runAutomata2 grammar rule "" = [False]
runAutomata2 grammar [] s = [False]

parseGrammar :: ParseTree -> Either String Grammar
parseGrammar tree = parseGrammarHelper tree >>= \rules -> Right $ array (0, 300) rules
  where
    parseGrammarHelper :: ParseTree -> Either String [(Int, Rule)]
    parseGrammarHelper (PSeq (PRule 7 tree) next) = (parseGrammarHelper tree)
    parseGrammarHelper (PSeq (PRule 0 rule) PEmpty) = do
      rule <- parseRule rule
      return [rule]
    parseGrammarHelper (PSeq (PRule 0 rule) (PSeq (PTerminal '\n') next)) = do
      rule <- parseRule rule
      rules <- parseGrammarHelper (PSeq next PEmpty)
      return (rule : rules)
    parseGrammarHelper tree = Left $ "Expected rule sequence, got " ++ (show tree)

    parseRule :: ParseTree -> Either String (Int, Rule)
    parseRule (PSeq (PNumber num) (PSeq (PTerminal ':' ) (PSeq (PTerminal ' ') body))) = parseChoice body >>= \pbody -> return (num, pbody)
    parseRule tree = Left $ "Expected rule, got " ++ (show tree)

    parseChoice :: ParseTree -> Either String Rule
    parseChoice (PRule 1 (PSeq seq (PRule 2 PEmpty))) = parseSeq seq
    parseChoice (PRule 1 (PSeq seq (PRule 2 (PSeq (PTerminal ' ') (PSeq (PTerminal '|') (PSeq (PTerminal ' ') choice)))))) = do
      pleft <- parseSeq seq
      pright <- parseChoice choice
      return $ Choice pleft pright
    parseChoice n = Left $ "Expected choice got " ++ (show n)

    parseSeq :: ParseTree -> Either String Rule
    parseSeq (PRule 3 (PSeq (PRule 5 term) (PRule 4 PEmpty))) = parseTerms term
    parseSeq (PRule 3 (PSeq (PRule 5 term) (PRule 4 (PSeq (PTerminal ' ') seq)))) = do
      pleft <- parseTerms term
      pright <- parseSeq seq
      return $ Seq pleft pright
    parseSeq n = Left $ "Expected sequence, got " ++ (show n)

    parseTerms :: ParseTree -> Either String Rule
    parseTerms (PNumber num) = Right $ (Expand num)
    parseTerms (PRule 6 (PSeq (PTerminal '"') (PSeq (PChar n) (PTerminal '"')))) = Right $ Terminal n
    parseTerms n = Left $ "Expected term, got " ++ (show n)

isCorrectSeq :: Grammar -> Rule -> String -> Bool
isCorrectSeq grammar rule str = any ((==) True) (runAutomata2 grammar [rule] str)

runGrammarParser :: Grammar -> Int -> String -> Either String Grammar
runGrammarParser grammar ruleNum str = case (runAutomata grammarRules (grammarRules ! ruleNum) str) of
  Left err -> Left err
  Right (leftStr, tree) -> if length leftStr > 0
    then Left ("Not all string parsed: " ++ leftStr)
    else parseGrammar tree

day19 :: IO Int
day19 = do
  contents <- readFile "day19.txt"
  let [grammarStr, exampleStr] = splitOn "\n\n" contents
  case runGrammarParser grammarRules 7 grammarStr of
    Left err -> ioError (userError err)
    Right grammar ->
      let examples = splitOn "\n" exampleStr in do
        return $ length (filter (isCorrectSeq grammar (grammar ! 0)) examples)

update :: [(Int, Rule)]
update = [
  (8, (Choice (Expand 42) (Seq (Expand 42) (Expand 8)))),
  (11, (Choice (Seq (Expand 42) (Expand 31)) (Seq (Expand 42) (Seq (Expand 11) (Expand 31)))))]

day19_2 :: IO Int
day19_2 = do
  contents <- readFile "day19.txt"
  let [grammarStr, exampleStr] = splitOn "\n\n" contents
  case runGrammarParser grammarRules 7 grammarStr of
    Left err -> ioError (userError err)
    Right grammar ->
      let examples = splitOn "\n" exampleStr in do
        return $ length (filter (isCorrectSeq (grammar//update) (grammar ! 0)) examples)