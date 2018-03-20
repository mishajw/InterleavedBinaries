module RegisterAllocation (allocate)
  where

import Data.List (isPrefixOf, break, nub, delete)
import Data.Text (replace, pack, unpack)
import Text.Regex (mkRegex, matchRegexAll)
import qualified Asm

-- | Allocate a set of registers to a series of instructions
allocate
  :: [String] -- ^ The registers to allocate across the instructions
  -> [Asm.Instruction] -- ^ The instructions to change the registers for
  -> [Asm.Instruction] -- ^ The instructions with modified registers
allocate registers instructions =
  if length registersUsed < length registers
  then
    Asm.insertAtLabel "main" criticalRegisterManagement $ map
      (\ins -> ins { Asm.arguments =
        map replaceRegisters (Asm.arguments ins)
      })
      instructions
  else
    error "Not enough registers to use for instructions" where

    criticalRegisterManagement :: [Asm.Instruction]
    criticalRegisterManagement =
      [Asm.Instruction "movq" ["%rbp", '%' : rbpReplacement] [],
       Asm.Instruction "movq" ["%rsp", '%' : rspReplacement] []]

    rbpReplacement = head registers
    rspReplacement = head $ tail registers

    registersMap :: [(String, String)]
    registersMap = (rbpReplacement, "rbp") : (rspReplacement, "rsp") :
                   zip registersUsed (drop 2 registers)

    -- | All registers used in the instruction
    -- Does not include `rbp` or `rsp` as these will be handled manually
    registersUsed :: [String]
    registersUsed = delete "rsp" . delete  "rbp" .  nub . concat $ concatMap
                    ((\(r, s) -> [r, s]) . getUsedRegisters)
                    instructions

    -- | Replace mentions of a register in a single argument
    replaceRegisters :: String -> String
    replaceRegisters s =
      foldr f s registersMap where
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
      if register == "rip"
      then getRegister rest
      else register : getRegister rest
    Nothing -> []

