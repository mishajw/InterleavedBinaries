module RegisterScope (
    RegScope (..),
    RegChangable (..),
    getScopes,
    getScopesInFunction)
  where

import Text.Read (readMaybe)
import Text.Regex (mkRegex, matchRegexAll)
import Data.Maybe (mapMaybe)
import Registers (Reg, SizedReg (..), parameterRegisters, returnRegister)
import qualified Asm

-- Whether a register was read from or written to
data RegUseType = RegRead | RegWrite deriving (Eq, Show)

-- | Flag for whether we can change a register
data RegChangable =
  -- The register can be changed to anything as long as it is consistent within
  -- the scope
  RegFlexible |
  -- The register must be mapped to the same value across the whole program. As
  -- long as another scope is flagged as this, the registers must be mapped to
  -- the same value. Used for passing parameters to functions
  RegLocalFixed |
  -- The register must stay as this value regardless. Used for syscalls
  RegGlobalFixed deriving (Eq, Ord, Show)

-- The usage of a register
data UsedReg =
  UsedReg
    Reg -- The register used
    RegUseType -- The way the register is used
    RegChangable -- How we are allowed to change the register used
    deriving (Show, Eq)

-- | Represents the scope of register usage
data RegScope = RegScope {
  startIndex :: Int,
  endIndex :: Int,
  register :: Reg,
  changable :: RegChangable,
  allocatedReg :: Maybe Reg
} deriving (Eq, Show)

getScopes :: Asm.Program -> [[RegScope]]
getScopes program = do
  func <- Asm.funcs program
  let instructions = Asm.instructions func
  return $ getScopesInFunction instructions

getScopesInFunction :: [Asm.Instruction] -> [RegScope]
getScopesInFunction instructions =
  -- Index all the instructions
  let indexedIns = zip [0..] instructions in
  -- Get where each register is used
  let
    indexedUsedRegs =
      map (\r -> (0, r)) functionBeginRegs ++
      concatMap (\(i, ins) -> map (\r -> (i, r)) (getUsedRegisters ins))
                indexedIns in
  -- Get all scopes
  let allScopes = create indexedUsedRegs in
  -- Remove base/stack pointer scopes because we handle them separately
  let
    filteredScopes = filter
      (\(RegScope _ _ r _ _) -> r `notElem` [read "bp", read "sp"])
      allScopes in
  -- Add in separate handling of base/stack pointer scopes
  criticalScopes ++ filteredScopes where

    create :: [(Int, UsedReg)] -> [RegScope]
    create [] = []
    create all@((i, usedReg@(UsedReg _ useType _)) : rest) =
      case getRegScope usedReg all of
          Just regScope ->
            let restWithoutReg = removeReferences regScope all in
            regScope : create restWithoutReg
          Nothing -> create rest

    criticalScopes :: [RegScope]
    criticalScopes =
      [RegScope 0 numInstructions (read "bp") RegLocalFixed Nothing,
       RegScope 0 numInstructions (read "sp") RegLocalFixed Nothing] where
      numInstructions = length instructions

    removeReferences :: RegScope -> [(Int, UsedReg)] -> [(Int, UsedReg)]
    removeReferences (RegScope start end reg _ _) =
      filter
      (\(i, UsedReg reg' _ _) -> not $ start <= i && i <= end && reg == reg')

getRegScope :: UsedReg -> [(Int, UsedReg)] -> Maybe RegScope
getRegScope ur@(UsedReg reg _ _) usedRegs =
  let relevantUsedRegs = filter (\(_, UsedReg reg' _ _) -> reg == reg')
                                usedRegs in
  case getUntilLastRead $ getUntilNextWrite relevantUsedRegs of
    [] -> Nothing
    usedRegsRange ->
      let (firstIndex, _) = head usedRegsRange in
      let (lastIndex, _) = last usedRegsRange in
      let changable = getChangable usedRegsRange in
      return $ RegScope firstIndex lastIndex reg changable Nothing
  where
    getUntilNextWrite :: [(Int, UsedReg)] -> [(Int, UsedReg)]
    getUntilNextWrite usedRegs' =
      head usedRegs' :
      takeWhile (\(_, UsedReg reg' useType _) ->
                   useType /= RegWrite) (tail usedRegs')
    getUntilLastRead :: [(Int, UsedReg)] -> [(Int, UsedReg)]
    getUntilLastRead usedRegs' =
      reverse $ dropWhile
                (\(i, UsedReg reg' useType _) ->
                  useType /= RegRead)
                (reverse usedRegs')
    getChangable :: [(Int, UsedReg)] -> RegChangable
    getChangable =
      -- We rely on the ordering of @RegChangable@ to get the most prevalent
      -- mode of changablility
      maximum . map (\(_, UsedReg _ _ changable) -> changable)

-- | Get registers used in an instruction
getUsedRegisters
  :: Asm.Instruction -- ^ The instruction using the registers
  -> [UsedReg] -- ^ Registers used in the instruction
getUsedRegisters (Asm.Instruction com args _ _)
  | com `elem` ["movq", "leaq", "movl"] =
    let [reads, writes] = args in
    getRegsInString RegRead reads ++ getRegsInString RegWrite writes
  | com `elem` ["subq", "addq"] =
    concatMap (getRegsInString RegRead) args
  | com `elem` ["call", "jmp"] = functionUsedRegs RegLocalFixed
  | com == "syscall" = functionUsedRegs RegGlobalFixed
  | otherwise = []

-- | The registers a function call can read from. Takes the changability of the
-- registers
functionUsedRegs :: RegChangable -> [UsedReg]
functionUsedRegs changable =
  UsedReg returnRegister RegWrite changable :
  map (\r -> UsedReg r RegRead changable) parameterRegisters

functionBeginRegs :: [UsedReg]
functionBeginRegs =
  map (\r -> UsedReg r RegWrite RegLocalFixed) parameterRegisters

-- | Get a list of registers mentioned in a string
-- TODO: Clean up double case of
getRegsInString :: RegUseType -> String -> [UsedReg]
getRegsInString defaultUseType s =
  let registerDerefRegex = mkRegex "\\(%([A-Za-z0-9]+)\\)" in
  let registerRegex = mkRegex "%([A-Za-z0-9]+)" in
  case (matchRegexAll registerDerefRegex s, matchRegexAll registerRegex s) of
    (Just (_, _, rest, [rString]), _) ->
      case readMaybe rString :: Maybe SizedReg of
        Just r -> UsedReg (reg r) RegRead RegFlexible :
                  getRegsInString defaultUseType rest
        Nothing -> getRegsInString defaultUseType rest
    (_, Just (_, _, rest, [rString])) ->
      case readMaybe rString :: Maybe SizedReg of
        Just r -> UsedReg (reg r) defaultUseType RegFlexible :
                  getRegsInString defaultUseType rest
        Nothing -> getRegsInString defaultUseType rest
    _ -> []

