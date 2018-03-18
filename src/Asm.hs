module Asm (
    Instruction (..),
    Program (..),
    stringToProgram,
    programToString)
  where

import Data.List.Split (splitOn)
import Data.List (intercalate, isPrefixOf, isInfixOf, isSuffixOf, elemIndex,
                  splitAt)
import Control.Monad.Except
import Data.Text (strip, pack, unpack)
import Text.Regex (mkRegex, matchRegex)

data Instruction = Instruction {
  command :: String,
  arguments :: [String],
  labels :: [String]
}

data Program = Program {
  instructions :: [Instruction],
  readOnlyData :: [Instruction]
}

type Result = Either String

-- | Create a program from an ASM string
stringToProgram :: String -> Result Program
stringToProgram rawInput = do
  let input = filterUnneeded .
              map (unpack . strip . pack) .
              splitOn "\n" $ rawInput
  (inputWithoutROData, readOnlyData) <- extractReadOnlyData input

  return $ Program
    (stringsToInstructions inputWithoutROData)
    (stringsToInstructions readOnlyData) where

    -- | Filter out ASM commands that are not needed
    filterUnneeded :: [String] -> [String]
    filterUnneeded lines =
      let conditions = [not . isPrefixOf ".file",
                        not . isPrefixOf ".size",
                        not . isPrefixOf ".ident",
                        not . isInfixOf ".note.GNU-stack"] in
      foldl (flip filter) lines conditions

    -- | Extract the section of ASM that contains read-only data
    extractReadOnlyData :: [String] -> Result ([String], [String])
    extractReadOnlyData lines = do
      startIndex <- case elemIndex ".section\t.rodata" lines of
        Just i -> return i
        Nothing -> Left "Couldn't find beginning of read only data"
      let (start, rest) = splitAt startIndex lines

      endIndex <- case elemIndex ".text" rest of
        Just i -> return i
        Nothing -> Left "Couldn't find end of read only data"
      let (readOnlyData, end) = splitAt (endIndex + 1) rest

      return (start ++ end, readOnlyData)

-- | Turn a program into ASM string
programToString :: Program -> String
programToString (Program instructions readOnly) =
  intercalate "\n"
  (instructionsToStrings readOnly ++ instructionsToStrings instructions)

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

