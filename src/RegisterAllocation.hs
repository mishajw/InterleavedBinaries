module RegisterAllocation (
    allocate,
    simpleAllocate)
  where

import Text.Regex (mkRegex, matchRegex, matchRegexAll)
import Data.Maybe (mapMaybe, isNothing)
import Data.List (delete, isInfixOf, nub)
import Data.Text (replace, pack, unpack, Text)
import qualified Asm
import Registers (Reg (..), SizedReg (..), Size (..))
import RegisterScope (RegScope (..), getScopes)

simpleAllocate
  :: [Reg] -- ^ List of registers we can use
  -> Asm.Program -- ^ Program to allocate registers to
  -> Asm.Program -- ^ Return program with changed registers
simpleAllocate regs prog =
    let [(_, bpReg)] = filter (\(r, _) -> r == read "bp") regMapping in
    let [(_, spReg)] = filter (\(r, _) -> r == read "sp") regMapping in
    insertCriticalRegisterManagement bpReg spReg .
    Asm.mapArgs replaceRegs $ prog where

    replaceRegs :: String -> String
    replaceRegs = replaceRegs' regStringMapping where
      replaceRegs' :: [(String, String)] -> String -> String
      replaceRegs' [] s = s
      replaceRegs' ((s1, s2) : rest) s
        | s1 `isInfixOf` s = unpack . replace (pack s1) (pack s2) . pack $ s
        | otherwise = replaceRegs' rest s

    regStringMapping :: [(String, String)]
    regStringMapping =
      concatMap
        (\(r1, r2) -> [
          (show $ SizedReg r1 Size64,
           show $ SizedReg r2 Size64),
          (show $ SizedReg r1 Size32,
           show $ SizedReg r2 Size32)])
        regMapping

    regMapping :: [(Reg, Reg)]
    regMapping = zip (nub allRegisters) regs

    allRegisters :: [Reg]
    allRegisters = do
      arg <- allArgs
      let regRegex = mkRegex "%([a-z0-9A-Z]+)"
      case matchRegex regRegex arg of
        Just [r] -> [reg (read r :: SizedReg)]
        Nothing -> []

    allArgs :: [String]
    allArgs = do
      func <- Asm.funcs prog
      instruction <- Asm.instructions func
      Asm.arguments instruction

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
      return $ filter
        -- Filter scopes for base/stack pointer as we're handling this
        -- separately
        (\(RegScope _ _ r _) -> r `notElem` [read "bp", read "sp"])
        (getScopes instructions)

    -- | Allocate the scopes for each function
    allocatedScopes :: [[RegScope]]
    allocatedScopes = map (allocateScopes usableRegs regFunc) functionScopes

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
        -- Append the base/stack pointer replacements
        (read "bp", bpReplacement) : (read "sp", spReplacement) :
        -- Map the scopes to their register replacements
        map (\(RegScope _ _ from (Just to)) -> (from, to))
        -- Filter for scopes that are applicable at this index
        (filter (\(RegScope s e _ _) -> s <= i && i <= e) scopes)

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
      -- ...and only select registers that have the same volatility as the
      -- source register...
      let
        sameVolatility =
          filter
            (\reg' -> (reg' `elem` volatileRegisters &&
                       reg `elem` volatileRegisters) ||
                      (reg' `elem` nonvolatileRegisters &&
                       reg `elem` nonvolatileRegisters))
            availableRegs in
      -- ...so we can select a register for this scope
      case sameVolatility of
        [] -> error "Could not find suitable register for scope"
        _ ->
          let (chosenReg, _) = regFunc availableRegs in
          RegScope start end reg (Just chosenReg)

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

