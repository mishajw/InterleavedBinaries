module NameResolver (postfixNames)
  where

import Text.Regex (mkRegex, matchRegexAll)
import qualified Asm

-- | Add some postfix to all names in a programm
postfixNames :: String -> Asm.Program -> Asm.Program
postfixNames postfix prog = prog {
  Asm.funcs = map postfixFunction (Asm.funcs prog),
  Asm.readOnlyData = map postfixRod (Asm.readOnlyData prog)
} where
  -- | Add postfixes to a function
  postfixFunction :: Asm.Func -> Asm.Func
  postfixFunction f = f {
    Asm.name = Asm.name f ++ postfix,
    Asm.instructions = map
                       (postfixLabels . postfixCalls . postfixRodAccess)
                       (Asm.instructions f)
  }

  -- | Add postfixes to a read only data section
  postfixRod :: Asm.ReadOnlyData -> Asm.ReadOnlyData
  postfixRod rod = rod {
    Asm.readOnlyLines = map postfixLabels (Asm.readOnlyLines rod)
  }

  -- | Add postfixes to an instruction's labels
  postfixLabels :: Asm.Instruction -> Asm.Instruction
  postfixLabels i = i {
    Asm.labels = map (++ postfix) (Asm.labels i)
  }

  -- | Add postfixes to function call label
  postfixCalls :: Asm.Instruction -> Asm.Instruction
  postfixCalls i
    | Asm.command i == "call" =
      i {Asm.arguments = map (++ postfix) (Asm.arguments i)}
    | otherwise = i

  -- | Add postfixes to a read only access
  postfixRodAccess :: Asm.Instruction -> Asm.Instruction
  postfixRodAccess i = i {
    Asm.arguments = map f (Asm.arguments i)
  } where
    f arg =
      let lcRegex = mkRegex "LC[0-9]+" in
      case matchRegexAll lcRegex arg of
        Just (before, matched, after, []) ->
          before ++ matched ++ postfix ++ f after
        Nothing -> arg

