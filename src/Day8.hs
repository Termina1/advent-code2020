module Day8
  (
    day8,
    day8_2 
  ) where

import Data.Array
import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as Set

data Instruction = Jmp Int | Acc Int | Nop Int
  deriving (Show)

type PcState = (Int, Int, Int)
type Program = Array Int Instruction
type PcResult = (Int, Bool)
type EvaluationStrategy = Instruction -> PcState -> (PcState -> PcResult) -> PcResult

parseProgram :: String ->  IO Program
parseProgram filename = do
  result <- parseFromFile programParser filename
  case result of
    Left err -> ioError (userError ("Invalid input: " ++ (show err)))
    Right program -> return program

instructionParser :: Parser Instruction
instructionParser = do
  name <- (string "jmp") <|> (string "acc") <|> (string "nop")
  space
  sig <- (char '+') <|> (char '-')
  dig <- many1 digit
  let num = if sig == '-' then -(read dig) else (read dig) in
    case name of
      "jmp" -> return $ Jmp num
      "acc" -> return $ Acc num
      "nop" -> return $ Nop num

programParser :: Parser Program
programParser = do
  instructions <- sepBy1 instructionParser (string "\n")
  return $ array (0, length instructions) (zip [0..] instructions)

executeCommand :: Instruction -> PcState -> PcState
executeCommand (Jmp num) (pc, acc, h) = (pc + num, acc, h)
executeCommand (Acc num) (pc, acc, h) = (pc + 1, acc + num, h)
executeCommand (Nop num) (pc, acc, h) = (pc + 1, acc, h)

shouldTerminate :: Int -> Program -> Bool
shouldTerminate val prog = let (start, end) = bounds prog in
  val < start || val >= end


deterministicEvaluator :: Instruction -> PcState -> (PcState -> PcResult) -> PcResult
deterministicEvaluator instr state next = next (executeCommand instr state)

holisticEvaluator :: Instruction -> PcState -> (PcState -> PcResult) -> PcResult
holisticEvaluator instr state@(_, _, 1) next = deterministicEvaluator instr state next
holisticEvaluator (Acc num) state next = deterministicEvaluator (Acc num) state next
holisticEvaluator instr state@(pc, acc, _) next = case deterministicEvaluator instr state next of
  (acc, True) -> (acc, True)
  (_, False) -> deterministicEvaluator (invert instr) (pc, acc, 1) next
  where
    invert :: Instruction -> Instruction
    invert (Jmp num) = Nop num
    invert (Nop num) = Jmp num
    invert instr = instr

execute :: Set.Set Int -> EvaluationStrategy -> Program -> PcState -> PcResult
execute visited evaluate program (pc, acc, h) =
  if Set.member pc visited 
    then (acc, False)
    else if shouldTerminate pc program 
      then (acc, True) 
      else evaluate (program ! pc) (pc, acc, h) (execute (Set.insert pc visited) evaluate program)

day8 :: IO Int
day8 = do 
  program <- parseProgram "day8.txt"
  let (acc, stopped) = execute Set.empty deterministicEvaluator program (0, 0, 0) in
    return acc

day8_2 :: IO Int
day8_2 = do 
  return 1
  program <- parseProgram "day8.txt"
  let (acc, stopped) = execute Set.empty holisticEvaluator program (0, 0, 0) in
    return acc