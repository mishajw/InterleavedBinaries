module StackReverse (reverseStack)
  where

import qualified Asm
import Text.Regex (mkRegex, matchRegexAll)
import Registers (Reg (..), SizedReg (..), Size (..))

-- | Reverse the stack usage in a program
-- TODO: Curren't doesn't work because `ret` and `call` need to be made implicit
-- in both the registers they use to push/pop, but also the direction of the
-- push/pop. This is an issue, because we need a spare register at these points,
-- which means we have to do scope checking first...
reverseStack :: Asm.Program -> Asm.Program
reverseStack prog = prog {
    Asm.funcs = map (\f -> f {
      Asm.instructions = map handleInstruction (Asm.instructions f)
    }) (Asm.funcs prog)
  } where

  -- | Reverse stack usage of an instruction
  handleInstruction :: Asm.Instruction -> Asm.Instruction
  handleInstruction = handleAllArguments . handleRegModification

  -- | Reverse stack usage of the arguments in an instruction
  handleAllArguments :: Asm.Instruction -> Asm.Instruction
  handleAllArguments ins = ins {
    Asm.arguments = map handleArgument (Asm.arguments ins)
  }

  -- | Reverse stack usage of a single argument
  handleArgument :: String -> String
  handleArgument s =
    let argRegex = mkRegex "(-?[0-9]+)\\(%([a-zA-Z0-9]+)\\)" in
    case matchRegexAll argRegex s of
      Just (before, _, after, [modifier, regName])
        | isStackRegister regName ->
          before ++ inverseNumber modifier ++ "(%" ++ regName ++ ")" ++
          handleArgument after
      _ -> s

  -- | Reverse a stack addition/subtraction
  handleRegModification :: Asm.Instruction -> Asm.Instruction
  handleRegModification (Asm.Instruction "addq" [num, '%' : regName] ls tied)
    | isStackRegister regName =
      Asm.Instruction "subq" [num, '%' : regName] ls tied
  handleRegModification (Asm.Instruction "subq" [num, '%' : regName] ls tied)
    | isStackRegister regName =
      Asm.Instruction "addq" [num, '%' : regName] ls tied
  handleRegModification i = i

  -- | Test whether a register is used for the stack
  isStackRegister :: String -> Bool
  isStackRegister s = s `elem` possibleNames where
    bp = read "bp" :: Reg
    sp = read "sp" :: Reg
    possibleNames =
      map show
      [SizedReg bp Size32, SizedReg sp Size32,
       SizedReg bp Size64, SizedReg sp Size64]

  -- | Inverse a number for stack referencing
  inverseNumber :: String -> String
  inverseNumber s = show $ -(read s :: Int)

