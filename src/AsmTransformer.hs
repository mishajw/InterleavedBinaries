module AsmTransformer (transform)
  where

import Data.List (isPrefixOf, break)
import qualified Asm

transform :: [Asm.Instruction] -> [Asm.Instruction]
transform = concatMap resolveImplicitRegister

resolveImplicitRegister :: Asm.Instruction -> [Asm.Instruction]
resolveImplicitRegister instruction@(Asm.Instruction com args labels) =
  case com of
    "pushq" -> [Asm.Instruction "subq" ["$8", "%rsp"] labels,
                Asm.Instruction "movq" (args ++ ["(%rsp)"]) []]
    "popq" -> [Asm.Instruction "movq" ("(%rsp)" : args) labels,
               Asm.Instruction "addq" ["$8", "%rsp"] []]
    "enter" -> resolveImplicitRegister (Asm.Instruction "pushq" ["%rbp"] []) ++
               [Asm.Instruction "movq" ["%rsp", "%rbp"] labels]
    "leave" -> Asm.Instruction "movq" ["%rbp", "%rsp"] labels :
               resolveImplicitRegister (Asm.Instruction "popq" ["%rbp"] [])
    _ -> [instruction]

