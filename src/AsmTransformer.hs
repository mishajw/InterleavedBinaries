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
    registers0 = ["rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rsp", "rbp"]
    registers1 :: [String]
    registers1 = map (("r" ++) . show) [8..15]

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

