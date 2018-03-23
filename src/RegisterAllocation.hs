module RegisterAllocation (
    allocate)
  where

import Text.Regex (mkRegex, matchRegex, matchRegexAll)
import Data.Maybe (mapMaybe, isNothing)
import Data.List (delete, isInfixOf, nub)
import Data.Text (replace, pack, unpack, Text)
import qualified Asm
import Registers (Reg (..), SizedReg (..), Size (..))
import RegisterScope (RegScope (..), RegChangable (..), getScopes)

-- | Allocate a set of registers to a series of instructions
allocate
  :: [Reg] -- ^ The registers to allocate across the instructions
  -> ([Reg] -> (Reg, [Reg])) -- ^ Function for getting the next register to use
  -> Asm.Program -- ^ The program to change the registers for
  -> Asm.Program -- ^ The program with modified registers
allocate regs regFunc program =
  -- Bind the register values
  let bindedProgram = program {
    Asm.funcs = zipWith bindFunction (Asm.funcs program) allocatedScopes
  } in
  -- Handle the base/stack pointer replacements
  insertCriticalRegisterManagement bpReplacement spReplacement bindedProgram
  where

    -- | Set replacement registers for base/stack pointers
    bpReplacement :: Reg
    spReplacement :: Reg
    usableRegs :: [Reg]
    ([bpReplacement, spReplacement], usableRegs) =
      takeNRegisters 2 regs regFunc

    -- | The initial register scopes in the program
    functionScopes :: [[RegScope]]
    functionScopes = do
      func <- Asm.funcs program
      let instructions = Asm.instructions func
      return $ getScopes instructions

    -- | Allocate the scopes for each function
    allocatedScopes :: [[RegScope]]
    allocatedScopes =
      map (allocateFlexibleScopes usableRegs regFunc) $
      allocateLocalFixedScopes usableRegs regFunc $
      allocateGlobalFixedScopes functionScopes

    -- | Bind all scoped variable in some function
    bindFunction :: Asm.Func -> [RegScope] -> Asm.Func
    bindFunction func scopes = func { Asm.instructions =
      newInstructions (Asm.instructions func)
    } where
      newInstructions :: [Asm.Instruction] -> [Asm.Instruction]
      newInstructions instructions =
        let indexedIns = zip [0..] instructions in
        map
          (\(i, ins) ->
            ins { Asm.arguments =
                    replaceRegs (getRegMap i) (Asm.arguments ins) } )
          indexedIns

      getRegMap :: Int -> [(Reg, Reg)]
      getRegMap i =
        -- Map the scopes to their register replacements
        map (\(RegScope _ _ from _ (Just to)) -> (from, to))
        -- Filter for scopes that are applicable at this index
        (filter (\(RegScope s e _ _ _) -> s <= i && i <= e) scopes)

allocateGlobalFixedScopes
  :: [[RegScope]]
  -> [[RegScope]]
allocateGlobalFixedScopes = map (map resolveGlobal) where
  resolveGlobal :: RegScope -> RegScope
  resolveGlobal (RegScope s e r RegGlobalFixed Nothing) =
    RegScope s e r RegGlobalFixed (Just r)
  resolveGlobal s = s

allocateLocalFixedScopes
  :: [Reg]
  -> ([Reg] -> (Reg, [Reg]))
  -> [[RegScope]]
  -> [[RegScope]]
allocateLocalFixedScopes regs regFunc = allocateLocalFixedScopes' regs where

  allocateLocalFixedScopes' :: [Reg] -> [[RegScope]] -> [[RegScope]]
  allocateLocalFixedScopes' [] allScopes = allScopes
  allocateLocalFixedScopes' (from : rest) allScopes =
    case getRegBinding allScopes from of
      Just to ->
        let binded = setRegBinding from to allScopes in
        allocateLocalFixedScopes' rest binded
      Nothing ->
        allocateLocalFixedScopes' rest allScopes

  setRegBinding :: Reg -> Reg -> [[RegScope]] -> [[RegScope]]
  setRegBinding from to = map (map set) where
    set :: RegScope -> RegScope
    set (RegScope s e r RegLocalFixed Nothing) | r == from =
      RegScope s e r RegLocalFixed (Just to)
    set scope = scope

  getRegBinding :: [[RegScope]] -> Reg -> Maybe Reg
  getRegBinding allScopes reg =
    case concatMap getPossibleInFunc allScopes of
      [] -> Nothing
      possibleRegs -> let (reg, _) = regFunc possibleRegs in Just reg
    where
    getPossibleInFunc :: [RegScope] -> [Reg]
    getPossibleInFunc scopes =
      let
        relevantScopes =
          filter (\s -> register s == reg && changable s == RegLocalFixed)
                 scopes in
      concatMap (\s -> possibleRegsForScope regs s scopes) relevantScopes

-- | Allocate register scopes within a series of instructions
allocateFlexibleScopes
  :: [Reg] -- | The registers to assign
  -> ([Reg] -> (Reg, [Reg])) -- | The register selection function
  -> [RegScope] -- | The scopes to assign registers to
  -> [RegScope] -- | Return the scopes with assigned registers
allocateFlexibleScopes regs regFunc = allocateFlexibleScopes' where
    allocateFlexibleScopes' :: [RegScope] -> [RegScope]
    allocateFlexibleScopes' scopes =
      let unresolvedScopes = filter (isNothing . allocatedReg) scopes in
      case unresolvedScopes of
        (scope : _) ->
          let otherScopes = delete scope scopes in
          let
            possibleRegs =
              possibleRegsForScope regs scope otherScopes in
          let (reg, _) = regFunc possibleRegs in
          let resolvedScope = scope { allocatedReg = Just reg } in
          allocateFlexibleScopes' $ resolvedScope : otherScopes
        [] -> scopes


-- | Get which register to use for a scope
possibleRegsForScope
  :: [Reg] -- ^ Registers to select from
  -> RegScope -- ^ The scope to find a register for
  -> [RegScope] -- ^ The other scopes in play
  -> [Reg] -- ^ Return the available registers
possibleRegsForScope
  regs (RegScope start end reg changable Nothing) otherScopes =
  -- Get all registers that are in use during the scope...
  let
    overlappingScopes =
      filter (\(RegScope s e _ _ _) ->
               (start <= s && s <= end) ||
               (start <= e && e <= end))
             otherScopes in
  let overlappingRegs = mapMaybe
                        (\(RegScope _ _ _ _ r) -> r)
                        overlappingScopes in
  -- ...and use this to select which registers are available...
  let availableRegs = filter (`notElem` overlappingRegs) regs in
  -- ...and only select registers that have the same volatility as the
  -- source register
  -- TODO: Is this step necessary with the new @RegChangable@ alg?
  filter
    (\reg' -> (reg' `elem` volatileRegisters &&
               reg `elem` volatileRegisters) ||
              (reg' `elem` nonvolatileRegisters &&
               reg `elem` nonvolatileRegisters))
    availableRegs
  where
    volatileRegisters :: [Reg]
    volatileRegisters = map read $ words "ax cx dx r8 r9 r10 r11"
    nonvolatileRegisters :: [Reg]
    nonvolatileRegisters = map read $ words "bx bp sp di si r12 r13 r14 r15"

-- | Handle critical register management by putting base/stack pointers into the
-- correct replacement registers
insertCriticalRegisterManagement :: Reg -> Reg -> Asm.Program -> Asm.Program
insertCriticalRegisterManagement bp sp =
  Asm.insertInFunction "_start" criticalRegisterManagement
  where
    criticalRegisterManagement :: [Asm.Instruction]
    criticalRegisterManagement =
      [Asm.Instruction
        "movq" ["%rbp", '%' : show (SizedReg bp Size64)] [],
       Asm.Instruction
        "movq" ["%rsp", '%' : show (SizedReg sp Size64)] []]

-- | Take N registers from the pool given some register function
takeNRegisters :: Int -> [Reg] -> ([Reg] -> (Reg, [Reg])) -> ([Reg], [Reg])
takeNRegisters n regs regFunc =
  foldr f ([], regs) [1..n] where
  f _ (selected, rest) =
    let (curSelected, curRest) = regFunc rest in
    (curSelected : selected, curRest)

-- | Replace all mentions of some registers with different registers
replaceRegs
  :: [(Reg, Reg)] -- ^ A map of registers to the registers to replace with
  -> [String] -- The strings to replace the registers in
  -> [String] -- Return the strings with replacements
replaceRegs regMap = map replaceSingle where
  replaceSingle :: String -> String
  replaceSingle s =
    let regRegex = mkRegex "%([A-Za-z0-9]+)" in
    case matchRegexAll regRegex s of
      Just (before, _, after, [regName]) ->
        before ++ "%" ++ replaceRegName regMap regName ++ replaceSingle after
      Nothing -> s

  replaceRegName :: [(Reg, Reg)] -> String -> String
  replaceRegName [] s = s
  replaceRegName ((r1, r2) : rest) s
    | s == show (SizedReg r1 Size64) = show (SizedReg r2 Size64)
    | s == show (SizedReg r1 Size32) = show (SizedReg r2 Size32)
    | otherwise = replaceRegName rest s

