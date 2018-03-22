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
  let registers = if i == 0 then registers0 else registers1 in
  RegisterAllocation.simpleAllocate registers explicitProg
  where
    registers0 :: [Reg]
    registers0 = map Reg ["ax", "bx", "cx", "dx", "si", "di", "sp", "bp"]
    registers1 :: [Reg]
    registers1 = map (Reg . ("r" ++) . show) [8..15]

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

