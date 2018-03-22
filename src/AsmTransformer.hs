module AsmTransformer (transform)
  where

import qualified Asm
import qualified RegisterAllocation
import Registers (Reg (..))

-- | Transform a list of instructions so it can be used in interleaving
transform
  :: Int -- ^ The index of the program (typically 0 or 1)
  -> Asm.Program -- ^ The program to transform
  -> Asm.Program -- ^ The transformed program
transform i prog =
  let explicitProg = resolveImplicitRegisters prog in
  let regs = if i == 0 then regs0 else regs1 in
  RegisterAllocation.allocate regs regFunc explicitProg
  where
    regs0 :: [Reg]
    regs0 = map Reg ["ax", "bx", "cx", "dx", "si", "di", "sp", "bp"]
    regs1 :: [Reg]
    regs1 = map (Reg . ("r" ++) . show) [8..15]
    regFunc :: [Reg] -> (Reg, [Reg])
    regFunc regs = (head regs, tail regs)

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

