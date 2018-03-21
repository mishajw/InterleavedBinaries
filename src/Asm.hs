module Asm (
    Instruction (..),
    Program (..),
    insertAtLabel,
    stringToProgram,
    programToString)
  where

import Data.List.Split (splitOn)
import Data.List (intercalate, isPrefixOf, isInfixOf, isSuffixOf, elemIndex,
                  splitAt, break)
import Control.Monad.Except
import Data.Text (strip, pack, unpack)
import Text.Regex (mkRegex, matchRegex)

data Instruction = Instruction {
  command :: String,
  arguments :: [String],
  labels :: [String]
} deriving Show

data Func = Func {
  name :: String,
  instructions :: [Instruction]
} deriving Show

newtype ReadOnlyData = ReadOnlyData {
  readOnlyLines :: [Instruction]
} deriving Show

data Program = Program {
  functions :: [Func],
  readOnlyData :: [ReadOnlyData]
} deriving Show

type Result = Either String

-- | Insert instructions at the top of some label
insertAtLabel
  :: String -- ^ The label to insert at
  -> [Asm.Instruction] -- ^ The instructions to insert at the label
  -> [Asm.Instruction] -- ^ The instructions to insert into
  -> [Asm.Instruction] -- ^ The instructions with inserted instructions
insertAtLabel label toInsert ins =
  let toInsertLabeled = (head toInsert) {Asm.labels = [label]} :
                            tail toInsert in
  let (preLabel, postLabel) = break (\a -> label `elem` Asm.labels a) ins in
  preLabel ++
    toInsertLabeled ++
    [(head postLabel) {Asm.labels = []}] ++
    tail postLabel

-- | Create a program from an ASM string
stringToProgram :: String -> Program
stringToProgram rawInput =
  let input = filterUnneeded . map (unpack . strip . pack) $ lines rawInput in
  let textLine : restInput = input in
  parseLines restInput where

    parseLines :: [String] -> Program
    parseLines [] = Program [] []
    parseLines (line : input) =
      case (matchRegex globlRegex line, matchRegex rodRegex line) of
        (Just [funcName], _) ->
          let (funcLines, _ : rest) = break (isInfixOf ".size") (tail input) in
          let func = Func funcName (stringsToInstructions funcLines) in
          let (Program funcs rods) = parseLines rest in
          Program (func : funcs) rods
        (_, Just []) ->
          let (rodLines, _ : rest) = break (== ".text") input in
          let rod = ReadOnlyData (stringsToInstructions rodLines) in
          let (Program funcs rods) = parseLines rest in
          Program funcs (rod : rods)
        _ -> error $ "Couldn't match expression: " ++ line

    globlRegex = mkRegex ".globl\t([a-zA-Z0-9_]+)"
    rodRegex = mkRegex ".section\t.rodata"

    -- | Filter out ASM commands that are not needed
    filterUnneeded :: [String] -> [String]
    filterUnneeded lines =
      let conditions = [not . isPrefixOf ".file",
                        not . isPrefixOf ".ident",
                        not . isInfixOf ".note.GNU-stack"] in
      foldl (flip filter) lines conditions

-- | Turn a program into ASM string
programToString :: Program -> String
programToString (Program funcs rods) =
  let
    rodLines =
      concatMap
        (\(ReadOnlyData ins) ->
          ".section\t.rodata" : instructionsToStrings ins ++ [".text"])
        rods in
  let
    funcLines =
      concatMap
        (\(Func name ins) ->
          (".globl " ++ name) :
          (".type " ++ name ++ ", @function") :
          instructionsToStrings ins ++
          [".size " ++ name ++ ", .-" ++ name])
      funcs in
  unlines $ rodLines ++ funcLines

stringsToInstructions :: [String] -> [Instruction]
stringsToInstructions = stringsToInstructions' [] where
  stringsToInstructions' :: [String] -> [String] -> [Instruction]
  stringsToInstructions' labels [] = []
  stringsToInstructions' labels (i : instructions) =
    let insRegex = mkRegex "^([a-zA-Z0-9]+)\t(.*)$" in
    let labelRegex = mkRegex "^(.*)\\:$" in
    case (matchRegex labelRegex i, matchRegex insRegex i) of
      (Just [label], _) -> stringsToInstructions' (label : labels) instructions
      (_, Just [command, arguments]) ->
        Instruction command (splitOn ", " arguments) labels :
        stringsToInstructions' [] instructions
      _ -> Instruction i [] labels : stringsToInstructions' [] instructions

instructionsToStrings :: [Instruction] -> [String]
instructionsToStrings = concatMap instructionToString

instructionToString :: Instruction -> [String]
instructionToString (Instruction com args labels) =
  map (++ ":") labels ++
  [com ++ "\t" ++ intercalate ", " args]

