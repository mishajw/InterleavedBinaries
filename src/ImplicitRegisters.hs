module ImplicitRegisters (makeExplicit)
  where

import Data.List (sort, sortBy, partition, (\\))
import qualified Asm
import qualified RegisterAllocation
import Registers (Reg (..), allRegisters, parameterRegisters, basePointer,
                  stackPointer)

makeExplicit :: Asm.Program -> Asm.Program
makeExplicit prog = prog {
    Asm.funcs = map resolveFunction (Asm.funcs prog)
  } where

  resolveFunction :: Asm.Func -> Asm.Func
  resolveFunction func = func {
    Asm.instructions = concatMap resolveInstruction (Asm.instructions func)
  }

  resolveInstruction instruction@(Asm.Instruction com args labels) =
    case com of
      "pushq" -> [Asm.Instruction "subq" ["$8", "%rsp"] labels,
                  Asm.Instruction "movq" (args ++ ["(%rsp)"]) []]
      "popq" -> [Asm.Instruction "movq" ("(%rsp)" : args) labels,
                 Asm.Instruction "addq" ["$8", "%rsp"] []]
      "enter" -> resolveInstruction (Asm.Instruction "pushq" ["%rbp"] []) ++
                 [Asm.Instruction "movq" ["%rsp", "%rbp"] labels]
      "leave" -> Asm.Instruction "movq" ["%rbp", "%rsp"] labels :
                 resolveInstruction (Asm.Instruction "popq" ["%rbp"] [])
      _ -> [instruction]

