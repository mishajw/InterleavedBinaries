module ImplicitRegisters (makeExplicit)
  where

import Data.List (sort, sortBy, partition, (\\))
import qualified Asm
import qualified RegisterAllocation
import Registers (Reg (..), allRegisters, parameterRegisters, basePointer,
                  stackPointer, SizedReg (..), Size (..))
import RegisterScope (RegScope (..), getScopesInFunction)

makeExplicit :: Asm.Program -> Asm.Program
makeExplicit prog = prog {
    Asm.funcs = map resolveFunction (Asm.funcs prog)
  } where

  resolveFunction :: Asm.Func -> Asm.Func
  resolveFunction func = func {
    Asm.instructions = concatMap
                         (uncurry resolveInstruction)
                         (zip [0..] $ Asm.instructions func)
  } where

    resolveInstruction :: Int -> Asm.Instruction -> [Asm.Instruction]
    resolveInstruction i instruction@(Asm.Instruction com args labels tied) =
      case com of
        "pushq" ->
          [Asm.Instruction "subq" ["$8", "%rsp"] labels tied,
           Asm.Instruction "movq" (args ++ ["(%rsp)"]) [] tied]
        "popq" ->
          [Asm.Instruction "movq" ("(%rsp)" : args) labels tied,
           Asm.Instruction "addq" ["$8", "%rsp"] [] tied]
        "enter" ->
          resolveInstruction i (Asm.Instruction "pushq" ["%rbp"] [] tied) ++
          [Asm.Instruction "movq" ["%rsp", "%rbp"] labels tied]
        "leave" ->
          Asm.Instruction "movq" ["%rbp", "%rsp"] labels tied :
          resolveInstruction i (Asm.Instruction "popq" ["%rbp"] [] tied)
        "call" ->
          Asm.Instruction
            "leaq" [spareLabel ++ "(%rip)", spareReg64] labels True :
          resolveInstruction i (Asm.Instruction "pushq" [spareReg64] [] True) ++
          [Asm.Instruction "jmp" args [] True,
           Asm.Instruction "addq" ["$8", "%rsp"] [spareLabel] tied]
        "ret" ->
          [Asm.Instruction "jmp" ["*(%rsp)"] labels tied]
        _ -> [instruction]

      where
        spareReg64 = '%' : show (SizedReg (getRegisterAtIndex i) Size64)
        spareLabel = "label_" ++ Asm.name func ++ "_" ++ show i

    scopes = getScopesInFunction (Asm.instructions func)

    getRegisterAtIndex :: Int -> Reg
    getRegisterAtIndex i =
      let relevantScopes = filter
                           (\s -> startIndex s <= i && i <= endIndex s)
                           scopes in
      let usedRegs = map register relevantScopes in
      let availableRegs = allRegisters \\ usedRegs in
      head availableRegs

