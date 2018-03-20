module RegisterAllocation (allocate)
  where

import Data.Maybe (mapMaybe, isNothing)
import Text.Read (readMaybe)
import Data.List (isPrefixOf, break, nub, delete)
import Data.Text (replace, pack, unpack)
import qualified Asm
import Registers (Reg (..), SizedReg (..), Size (..))
import RegisterScope (RegScope (..), getScopes)

-- | Allocate a set of registers to a series of instructions
allocate
  :: [Reg] -- ^ The registers to allocate across the instructions
  -> ([Reg] -> (Reg, [Reg])) -- ^ Function for getting the next register to use
  -> [Asm.Instruction] -- ^ The instructions to change the registers for
  -> [Asm.Instruction] -- ^ The instructions with modified registers
allocate regs regFunc instructions =
  let allocatedScopes = allocateScopes scopes in
  let binded = foldr bindScope criticalHandledInstructions allocatedScopes in
  Asm.insertAtLabel "main" criticalRegisterManagement binded
  where
    scopes :: [RegScope]
    scopes =
      filter
      (\(RegScope _ _ r _) -> r `notElem` [read "bp", read "sp"])
      (getScopes instructions)

    criticalHandledInstructions :: [Asm.Instruction]
    criticalHandledInstructions =
      replaceRegs (read "sp") spReplacement .
      replaceRegs (read "bp") bpReplacement $ instructions

    allocateScopes :: [RegScope] -> [RegScope]
    allocateScopes scopes =
      let
        unresolvedScopes =
          filter (\(RegScope _ _ _ setReg) -> isNothing setReg)
                 scopes in
      case unresolvedScopes of
        (scope : _) ->
          let otherScopes = delete scope scopes in
          let resolvedScope = regForScope scope otherScopes in
          allocateScopes $ resolvedScope : otherScopes
        [] -> scopes

    criticalRegisterManagement :: [Asm.Instruction]
    criticalRegisterManagement =
      [Asm.Instruction
        "movq" ["%rbp", '%' : show (SizedReg bpReplacement Size64)] [],
       Asm.Instruction
        "movq" ["%rsp", '%' : show (SizedReg spReplacement Size64)] []]

    (bpReplacement, (spReplacement, useableRegs)) =
      let (bp, rest) = regFunc regs in
      (bp, regFunc rest)

    -- | Get which register to use for a scope
    regForScope
      :: RegScope -- ^ The scope to find a register for
      -> [RegScope] -- ^ The other scopes in play
      -> RegScope -- ^ The scope with a fixed register
    regForScope
      (RegScope start end reg Nothing) otherScopes =
      let
        overlappingScopes =
          filter (\(RegScope s e _ _) ->
                   (start <= s && s <= end) ||
                   (start <= e && e <= end))
                 otherScopes in
      let overlappingRegs = mapMaybe
                            (\(RegScope _ _ _ r) -> r)
                            overlappingScopes in
      let availableRegs = filter (`notElem` overlappingRegs) useableRegs in
      let (chosenReg, _) = regFunc availableRegs in
      RegScope start end reg (Just chosenReg)

    bindScope :: RegScope -> [Asm.Instruction] -> [Asm.Instruction]
    bindScope rs@(RegScope start end from (Just to)) ins =
      let (beginning, rest) = splitAt start ins in
      let (middle, ending) = splitAt (end - start + 1) rest in
      beginning ++ replaceRegs from to middle ++ ending

    replaceRegs :: Reg -> Reg -> [Asm.Instruction] -> [Asm.Instruction]
    replaceRegs from to = map f where
      f i = i {
        -- Map all arguments to replace the registers
        Asm.arguments =
          map
          (unpack .
            -- Replace all 64 bit register mentions
            replace
              (pack . show $ SizedReg from Size64)
              (pack . show $ SizedReg to Size64) .
            -- Replace all 32 bit register mentions
            replace
              (pack . show $ SizedReg from Size32)
              (pack . show $ SizedReg to Size32) .
            pack)
          (Asm.arguments i)
      }

