module NameResolver (postfixNames)
  where

import Data.Text (pack, unpack, replace)
import Data.Char (isAlphaNum)
import Text.Regex (mkRegex, matchRegexAll)
import qualified Asm

-- | Add some postfix to all names in a programm
postfixNames :: String -> Asm.Program -> Asm.Program
postfixNames postfix prog = prog {
  Asm.funcs = map handleFunc (Asm.funcs prog),
  Asm.readOnlyData = map handleRod (Asm.readOnlyData prog)
} where

  allLabels :: [String]
  allLabels = funcLabels ++ rodLabels where
    funcLabels = do
      func <- Asm.funcs prog
      ins <- Asm.instructions func
      Asm.labels ins
    rodLabels = do
      rod <- Asm.readOnlyData prog
      ins <- Asm.readOnlyLines rod
      Asm.labels ins

  handleRenames :: String -> String
  handleRenames s = foldr f s allLabels where
    f label s' =
      unpack $ replace (pack label) (pack (label ++ postfix)) (pack s')

  -- | Add postfixes to a function
  handleFunc :: Asm.Func -> Asm.Func
  handleFunc f = f {
    Asm.name = handleRenames $ Asm.name f,
    Asm.instructions = map handleInstruction (Asm.instructions f)
  }

  handleInstruction :: Asm.Instruction -> Asm.Instruction
  handleInstruction ins = ins {
    Asm.arguments = map handleRenames (Asm.arguments ins),
    Asm.labels = map handleRenames (Asm.labels ins)
  }

  handleRod :: Asm.ReadOnlyData -> Asm.ReadOnlyData
  handleRod rod = rod {
    Asm.readOnlyLines = map handleInstruction (Asm.readOnlyLines rod)
  }
