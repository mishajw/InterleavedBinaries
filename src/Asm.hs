module Asm (
    Instruction,
    Program,
    stringToProgram)
  where

import Data.List.Split (splitOn)

type Instruction = String
type Program = [Instruction]

stringToProgram :: String -> Program
stringToProgram = splitOn "\n"

