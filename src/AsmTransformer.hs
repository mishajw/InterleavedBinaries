module AsmTransformer (transform)
  where

import Data.List (sort, sortBy)
import qualified Asm
import qualified RegisterAllocation
import Registers (Reg (..), allRegisters)

-- | Transform a list of instructions so it can be used in interleaving
transform
  :: Int -- ^ The index of the program (typically 0 or 1)
  -> Asm.Program -- ^ The program to transform
  -> Asm.Program -- ^ The transformed program
transform i prog =
  let explicitProg = resolveImplicitRegisters prog in
  let regFunc = if i == 0
                then regFunc' sort
                else regFunc' (sortBy (flip compare)) in
  RegisterAllocation.allocate allRegisters regFunc explicitProg
  where
    regFunc' :: ([Reg] -> [Reg]) -> [Reg] -> (Reg, [Reg])
    regFunc' f regs = let fRegs = f regs in (head fRegs, tail fRegs)

resolveImplicitRegisters :: Asm.Program -> Asm.Program
resolveImplicitRegisters prog = prog {
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

