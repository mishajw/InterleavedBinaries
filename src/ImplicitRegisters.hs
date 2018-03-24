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
    resolveInstruction i instruction@(Asm.Instruction com args labels) =
      case com of
        "pushq" ->
          [Asm.Instruction "subq" ["$8", "%rsp"] labels,
           Asm.Instruction "movq" (args ++ ["(%rsp)"]) []]
        "popq" ->
          [Asm.Instruction "movq" ("(%rsp)" : args) labels,
           Asm.Instruction "addq" ["$8", "%rsp"] []]
        "enter" ->
          resolveInstruction i (Asm.Instruction "pushq" ["%rbp"] []) ++
          [Asm.Instruction "movq" ["%rsp", "%rbp"] labels]
        "leave" ->
          Asm.Instruction "movq" ["%rbp", "%rsp"] labels :
          resolveInstruction i (Asm.Instruction "popq" ["%rbp"] [])
        "call" ->
          Asm.Instruction "leaq" [spareLabel ++ "(%rip)", spareReg64] labels :
          resolveInstruction i (Asm.Instruction "pushq" [spareReg64] []) ++
          [Asm.Instruction "jmp" args [],
           Asm.Instruction "addq" ["$8", "%rsp"] [spareLabel]]
        "ret" ->
          [Asm.Instruction "jmp" ["*(%rsp)"] labels]
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

