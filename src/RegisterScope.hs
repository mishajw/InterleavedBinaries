module RegisterScope (
    RegScope (..),
    getScopes)
  where

import Text.Read (readMaybe)
import Text.Regex (mkRegex, matchRegexAll)
import Data.Maybe (mapMaybe)
import Registers (Reg, SizedReg (..))
import qualified Asm

-- | Represents a register being used, and whether it must use the same value
data RegUseType = RegRead | RegWrite deriving Eq
data UsedReg =
  UsedReg
    Reg -- The register used
    RegUseType -- The way the register is used
    (Maybe Reg) -- What the register should be set to
    deriving Eq

-- | Represents the scope of register usage
data RegScope = RegScope {
  startIndex :: Int,
  endIndex :: Int,
  register :: Reg,
  allocatedReg :: Maybe Reg
} deriving Eq

getScopes :: [Asm.Instruction] -> [RegScope]
getScopes instructions =
  let indexedIns = zip [0..] instructions in
  let
    indexedUsedRegs =
      concatMap (\(i, ins) -> map (\r -> (i, r)) (getUsedRegisters ins))
                indexedIns in
    create indexedUsedRegs where

    create :: [(Int, UsedReg)] -> [RegScope]
    create [] = []
    create all@((i, usedReg@(UsedReg _ useType _)) : rest) =
      case getRegScope usedReg all of
          Just regScope ->
            let restWithoutReg = removeReferences regScope all in
            regScope : create restWithoutReg
          Nothing -> create rest

    removeReferences :: RegScope -> [(Int, UsedReg)] -> [(Int, UsedReg)]
    removeReferences (RegScope start end reg _) =
      filter
      (\(i, UsedReg reg' _ _) -> not $ start <= i && i <= end && reg == reg')

getRegScope :: UsedReg -> [(Int, UsedReg)] -> Maybe RegScope
getRegScope ur@(UsedReg reg _ _) usedRegs =
  case getUntilLastRead $ getUntilNextWrite usedRegs of
    [] -> Nothing
    usedRegsRange ->
      let (firstIndex, _) = head usedRegsRange in
      let (lastIndex, _) = last usedRegsRange in
      return $ RegScope firstIndex lastIndex reg (getAllocatedReg usedRegsRange)
  where
    getUntilNextWrite :: [(Int, UsedReg)] -> [(Int, UsedReg)]
    getUntilNextWrite usedRegs' =
      head usedRegs' :
      takeWhile (\(_, UsedReg reg' useType _) ->
                   reg /= reg' || useType /= RegWrite) (tail usedRegs')
    getUntilLastRead :: [(Int, UsedReg)] -> [(Int, UsedReg)]
    getUntilLastRead usedRegs' =
      reverse $ dropWhile
                (\(i, UsedReg reg' useType _) ->
                  reg /= reg' || useType /= RegRead)
                (reverse usedRegs')
    getAllocatedReg :: [(Int, UsedReg)] -> Maybe Reg
    getAllocatedReg usedRegs' =
      let relevantUsedRegs = filter
                             (\(_, UsedReg reg' _ _) -> reg == reg')
                             usedRegs' in
      case mapMaybe (\(_, UsedReg _ _ reg') -> reg') relevantUsedRegs of
        [] -> Nothing
        allocatedReg : _ -> Just allocatedReg

-- | Get registers used in an instruction
getUsedRegisters
  :: Asm.Instruction -- ^ The instruction using the registers
  -> [UsedReg] -- ^ Registers used in the instruction
getUsedRegisters (Asm.Instruction com args _)
  | com `elem` ["movq", "leaq", "movl"] =
    let [reads, writes] = args in
    getRegsInString RegRead reads ++ getRegsInString RegWrite writes
  | com `elem` ["subq", "addq"] =
    concatMap (getRegsInString RegRead) args
  | com `elem` ["call", "syscall"] = functionUsedRegs
  | otherwise = []

-- | The registers a function call can read from
functionUsedRegs :: [UsedReg]
functionUsedRegs =
  UsedReg (read "ax") RegWrite (Just $ read "ax") :
  map
  (\s -> UsedReg (read s) RegRead (Just $ read s))
  ["si", "di", "dx", "cx", "r8", "r9"]

-- | Get a list of registers mentioned in a string
-- TODO: Clean up double case of
getRegsInString :: RegUseType -> String -> [UsedReg]
getRegsInString defaultUseType s =
  let registerDerefRegex = mkRegex "\\(%([A-Za-z0-9]+)\\)" in
  let registerRegex = mkRegex "%([A-Za-z0-9]+)" in
  case (matchRegexAll registerDerefRegex s, matchRegexAll registerRegex s) of
    (Just (_, _, rest, [rString]), _) ->
      case readMaybe rString :: Maybe SizedReg of
        Just r -> UsedReg (reg r) RegRead Nothing :
                  getRegsInString defaultUseType rest
        Nothing -> getRegsInString defaultUseType rest
    (_, Just (_, _, rest, [rString])) ->
      case readMaybe rString :: Maybe SizedReg of
        Just r -> UsedReg (reg r) defaultUseType Nothing :
                  getRegsInString defaultUseType rest
        Nothing -> getRegsInString defaultUseType rest
    _ -> []

