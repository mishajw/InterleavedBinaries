module AsmTransformer (transform)
  where

import Data.List (isPrefixOf, break, nub, delete)
import Data.Text (replace, pack, unpack)
import Text.Regex (mkRegex, matchRegexAll)
import qualified Asm

-- | Transform a list of instructions so it can be used in interleaving
transform
  :: Int -- ^ The index of the program (typically 0 or 1)
  -> [Asm.Instruction] -- ^ The instruction to transform
  -> [Asm.Instruction] -- ^ The transformed instruction
transform i =
  let registers = if i == 0 then registers0 else registers1 in
  allocateRegisters registers . concatMap resolveImplicitRegister
  where
    registers0 :: [String]
    registers0 = ["eax", "ebx", "ecx", "edx", "esi", "edi", "esp", "ebp"]
    registers1 :: [String]
    registers1 = map ((++ "d") . ("r" ++) . show) [8..15]

resolveImplicitRegister :: Asm.Instruction -> [Asm.Instruction]
resolveImplicitRegister instruction@(Asm.Instruction com args labels) =
  case com of
    "pushl" -> [Asm.Instruction "subl" ["$8", "%esp"] labels,
                Asm.Instruction "movl" (args ++ ["(%esp)"]) []]
    "popl" -> [Asm.Instruction "movl" ("(%esp)" : args) labels,
               Asm.Instruction "addl" ["$8", "%esp"] []]
    "enter" -> resolveImplicitRegister (Asm.Instruction "pushl" ["%ebp"] []) ++
               [Asm.Instruction "movl" ["%esp", "%ebp"] labels]
    "leave" -> Asm.Instruction "movl" ["%ebp", "%esp"] labels :
               resolveImplicitRegister (Asm.Instruction "popl" ["%ebp"] [])
    _ -> [instruction]

-- | Allocate a different set of registers to a sequence of instructions
allocateRegisters
  :: [String] -- ^ Pool of registers to use
  -> [Asm.Instruction] -- ^ The instructions to reallocate registers for
  -> [Asm.Instruction] -- ^ The instructions with reallocated registers
allocateRegisters registers instructions =
  if length registersUsed < length registers
  then
    insertAtMain criticalRegisterManagement $ map
      (\ins -> ins { Asm.arguments =
        map replaceRegisters (Asm.arguments ins)
      })
      instructions
  else
    error "Not enough registers to use for instructions" where

    -- | Insert instructions at the top of the main function
    insertAtMain
      :: [Asm.Instruction] -- ^ The instructions to insert at main
      -> [Asm.Instruction] -- ^ The instructions to insert into
      -> [Asm.Instruction] -- ^ The instructions with inserted instructions
    insertAtMain toInsert ins =
      let toInsertLabeledMain = (head toInsert) {Asm.labels = ["main"]} :
                                tail toInsert in
      let (preMain, postMain) = break (\a -> "main" `elem` Asm.labels a) ins in
      preMain ++
        toInsertLabeledMain ++
        [(head postMain) {Asm.labels = []}] ++
        tail postMain

    criticalRegisterManagement :: [Asm.Instruction]
    criticalRegisterManagement =
      [Asm.Instruction "movl" ["%ebp", '%' : rbpReplacement] [],
       Asm.Instruction "movl" ["%esp", '%' : rspReplacement] []]

    rbpReplacement = head registers
    rspReplacement = head $ tail registers

    registersMap :: [(String, String)]
    registersMap = ("ebp", rbpReplacement) : ("esp", rspReplacement) :
                   zip registersUsed (drop 2 registers)

    -- | All registers used in the instruction
    -- Does not include `rbp` or `rsp` as these will be handled manually
    registersUsed :: [String]
    registersUsed = delete "esp" . delete "ebp" . nub . concat $ concatMap
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
  if com `elem` ["movl", "subl", "addl", "leal"]
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
      if register == "eip"
      then getRegister rest
      else register : getRegister rest
    Nothing -> []

