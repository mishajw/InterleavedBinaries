module Asm (
    Instruction,
    Program,
    stringToProgram,
    programToString)
  where

import Data.List.Split (splitOn)
import Data.List (intercalate)

type Instruction = String
type Program = [Instruction]

stringToProgram :: String -> Program
stringToProgram = splitOn "\n"

programToString :: Program -> String
programToString = intercalate "\n"

