module AsmTransformer (transform)
  where

import Data.List (isPrefixOf, break)
import Data.Text (replace, pack, unpack)
import Text.Regex (mkRegex, matchRegexAll)
import qualified Asm

transform :: [Asm.Instruction] -> [Asm.Instruction]
transform = allocateRegisters registers . concatMap resolveImplicitRegister
  where
    registers :: [String]
    registers = map (("r" ++) . show) [8..15]

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

-- | Allocate a different set of registers to a sequence of instructions
allocateRegisters
  :: [String] -- ^ Pool of registers to use
  -> [Asm.Instruction] -- ^ The instructions to reallocate registers for
  -> [Asm.Instruction] -- ^ The instructions with reallocated registers
allocateRegisters registers instructions =
  if length registersUsed > length registers
  then
    map
      (\ins -> ins { Asm.arguments =
        map replaceRegisters (Asm.arguments ins)
      })
      instructions
  else
    error "Not enough registers to use for instructions" where

    -- | All registers used in the instruction
    registersUsed :: [String]
    registersUsed = concat $ concatMap
                    ((\(r, s) -> [r, s]) . getUsedRegisters)
                    instructions

    -- | Replace mentions of a register in a single argument
    replaceRegisters :: String -> String
    replaceRegisters s =
      foldr f s (zip registersUsed registers) where
      f :: (String, String) -> String -> String
      f (ru, r) s = unpack $ replace (pack ru) (pack r) (pack s)

-- | Get registers used in an instruction
getUsedRegisters
  :: Asm.Instruction -- ^ The instruction using the registers
  -> ([String], [String]) -- ^ Tuple of (registers read, registers written)
getUsedRegisters (Asm.Instruction com args _) =
  if com `elem` ["movq", "subq", "addq", "leaq"]
  then
    let [reads, writes] = args in
    (getRegister reads, getRegister writes)
  else
    ([], [])

-- | Get a list of registers mentioned in a string
getRegister :: String -> [String]
getRegister s =
  let registerRegex = mkRegex "%([A-Za-z0-9]+)" in
  case matchRegexAll registerRegex s of
    Just (_, _, rest, [register]) ->
      register : getRegister rest
    Nothing -> []

