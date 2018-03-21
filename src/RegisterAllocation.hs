module RegisterAllocation (allocate)
  where

import Data.Maybe (mapMaybe, isNothing)
import Data.List (delete)
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
  -- 1) Select registers for base/stack pointers
  let ([bpReplacement, spReplacement], regs) = takeNRegisters 2 regs regFunc in
  -- 2) Allocate the scoped registers to new registers
  let allocatedScopes = allocateScopes regs regFunc initialScopes in
  -- 3) Bind the allocated scopes
  let binded = foldr bindScope instructions allocatedScopes in
  -- 4) Insert the critical instructions at the beginning of the main function
  handleCriticalRegisters bpReplacement spReplacement binded
  where
    -- The initial register scopes in the program
    initialScopes :: [RegScope]
    initialScopes =
      filter
      (\(RegScope _ _ r _) -> r `notElem` [read "bp", read "sp"])
      (getScopes instructions)

    -- Bind all register references within a scope
    bindScope :: RegScope -> [Asm.Instruction] -> [Asm.Instruction]
    bindScope rs@(RegScope start end from (Just to)) ins =
      let (beginning, rest) = splitAt start ins in
      let (middle, ending) = splitAt (end - start + 1) rest in
      beginning ++ replaceRegs from to middle ++ ending

-- | Allocate register scopes within a series of instructions
allocateScopes
  :: [Reg] -- | The registers to assign
  -> ([Reg] -> (Reg, [Reg])) -- | The register selection function
  -> [RegScope] -- | The scopes to assign registers to
  -> [RegScope] -- | Return the scopes with assigned registers
allocateScopes regs regFunc = allocateScopes' where
    allocateScopes' :: [RegScope] -> [RegScope]
    allocateScopes' scopes =
      let
        unresolvedScopes =
          filter (\(RegScope _ _ _ setReg) -> isNothing setReg)
                 scopes in
      case unresolvedScopes of
        (scope : _) ->
          let otherScopes = delete scope scopes in
          let resolvedScope = regForScope scope otherScopes in
          allocateScopes' $ resolvedScope : otherScopes
        [] -> scopes

    -- | Get which register to use for a scope
    regForScope
      :: RegScope -- ^ The scope to find a register for
      -> [RegScope] -- ^ The other scopes in play
      -> RegScope -- ^ The scope with a fixed register
    regForScope
      (RegScope start end reg Nothing) otherScopes =
      -- Get all registers that are in use during the scope...
      let
        overlappingScopes =
          filter (\(RegScope s e _ _) ->
                   (start <= s && s <= end) ||
                   (start <= e && e <= end))
                 otherScopes in
      let overlappingRegs = mapMaybe
                            (\(RegScope _ _ _ r) -> r)
                            overlappingScopes in
      -- ...and use this to select which registers are available...
      let availableRegs = filter (`notElem` overlappingRegs) regs in
      -- ...so we can select a register for this scope
      let (chosenReg, _) = regFunc availableRegs in
      RegScope start end reg (Just chosenReg)

-- Perform critical register management by:
-- 1) Replacing the stack/base pointer with other registers
-- 2) Assigning original stack/base pointers to their replacements
handleCriticalRegisters
  :: Reg -- ^ The register to use for the base pointer
  -> Reg -- ^ The register to use for the stack pointer
  -> [Asm.Instruction] -- ^ The instructions to modify
  -> [Asm.Instruction] -- ^ The modified instructions
handleCriticalRegisters bpReplacement spReplacement instructions =
    Asm.insertAtLabel "main" criticalRegisterManagement replacedInstructions
  where
    -- The instructions where the critical registers have been replaced
    replacedInstructions :: [Asm.Instruction]
    replacedInstructions =
      replaceRegs (read "sp") spReplacement .
      replaceRegs (read "bp") bpReplacement $ instructions

    -- | The instructions which handle the critical registers - in this case,
    -- assign the stack/base pointer replacements to start with the correct
    -- values
    criticalRegisterManagement :: [Asm.Instruction]
    criticalRegisterManagement =
      [Asm.Instruction
        "movq" ["%rbp", '%' : show (SizedReg bpReplacement Size64)] [],
       Asm.Instruction
        "movq" ["%rsp", '%' : show (SizedReg spReplacement Size64)] []]

-- | Take N registers from the pool given some register function
takeNRegisters :: Int -> [Reg] -> ([Reg] -> (Reg, [Reg])) -> ([Reg], [Reg])
takeNRegisters n regs regFunc =
  foldr f ([], regs) [0..n] where
  f _ (selected, rest) =
    let (curSelected, curRest) = regFunc rest in
    (curSelected : selected, curRest)

-- Replace all mentions of a register with another register in some
-- instructions
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

