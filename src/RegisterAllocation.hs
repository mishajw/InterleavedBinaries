module RegisterAllocation (allocate)
  where

import Text.Read (readMaybe)
import Data.List (isPrefixOf, break, nub, delete)
import Data.Text (replace, pack, unpack)
import Text.Regex (mkRegex, matchRegexAll)
import qualified Asm
import Registers (Reg (..), SizedReg (..), Size (..))

-- | Allocate a set of registers to a series of instructions
allocate
  :: [Reg] -- ^ The registers to allocate across the instructions
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
      [Asm.Instruction
        "movq" ["%rbp", '%' : show (SizedReg bpReplacement Size64)] [],
       Asm.Instruction
        "movq" ["%rsp", '%' : show (SizedReg spReplacement Size64)] []]

    bpReplacement :: Reg
    bpReplacement = head registers
    spReplacement :: Reg
    spReplacement = head $ tail registers

    registersMap :: [(Reg, Reg)]
    registersMap = (Reg "bp", bpReplacement) : (Reg "sp", spReplacement) :
                   zip registersUsed (drop 2 registers)

    -- | All registers used in the instruction
    -- Does not include base or stack pointer registers as these will be handled
    -- manually
    registersUsed :: [Reg]
    registersUsed =
      delete (Reg "sp") . delete (Reg "bp") . nub . concat $ concatMap
      ((\(r, s) -> [r, s]) . getUsedRegisters)
      instructions

    -- | Replace mentions of a register in a single argument
    replaceRegisters :: String -> String
    replaceRegisters s =
      foldr f s registerStringMap where
      f :: (String, String) -> String -> String
      f (ru, r) s = unpack $ replace (pack ru) (pack r) (pack s)
      registerStringMap :: [(String, String)]
      registerStringMap =
        concatMap (\(ru, r) ->
          [(show (SizedReg ru Size64), show (SizedReg r Size64)),
           (show (SizedReg ru Size32), show (SizedReg r Size32))]) registersMap

-- | Get registers used in an instruction
getUsedRegisters
  :: Asm.Instruction -- ^ The instruction using the registers
  -> ([Reg], [Reg]) -- ^ Tuple of (registers read, registers written)
getUsedRegisters (Asm.Instruction com args _) =
  if com `elem` ["movq", "subq", "addq", "leaq"]
  then
    let [reads, writes] = args in
    (getRegister reads, getRegister writes)
  else
    ([], [])

-- | Get a list of registers mentioned in a string
getRegister :: String -> [Reg]
getRegister s =
  let registerRegex = mkRegex "%([A-Za-z0-9]+)" in
  case matchRegexAll registerRegex s of
    Just (_, _, rest, [rString]) ->
      case readMaybe rString :: Maybe SizedReg of
        Just r -> reg r : getRegister rest
        Nothing -> getRegister rest
    Nothing -> []

