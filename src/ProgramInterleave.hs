module ProgramInterleave (
  interleave,
  simpleInterleave)
  where

import Text.Regex (mkRegex, matchRegexAll)
import Data.List (union, partition)
import qualified Asm

simpleInterleave :: Asm.Program -> Asm.Program -> Asm.Program
simpleInterleave prog1 prog2 =
  -- Get the main and start functions
  -- We discard one of the start functions because they (should be) identical
  let prog1' = postfixNames "_1" prog1 in
  let prog2' = postfixNames "_2" prog2 in
  let (start1, funcs1) = getFunc "_start_1" (Asm.funcs prog1') in
  let (start2, funcs2) = getFunc "_start_2" (Asm.funcs prog2') in
  Asm.Program {
    Asm.funcs = interleaveFunc start1 start2 : funcs1 ++ funcs2,
    Asm.readOnlyData = Asm.readOnlyData prog1' ++ Asm.readOnlyData prog2'
 }
  where
    getFunc :: String -> [Asm.Func] -> (Asm.Func, [Asm.Func])
    getFunc funcName fs =
      let ([start], others) = partition (\f -> Asm.name f == funcName) fs in
      (start, others)

    interleaveFunc :: Asm.Func -> Asm.Func -> Asm.Func
    interleaveFunc f1 f2 =
      Asm.Func {
        Asm.name = Asm.name f1,
        Asm.instructions =
          concat $
          zipWith combineInstructions
            (Asm.instructions f1) (Asm.instructions f2)
      }


interleave :: [Asm.Instruction] -> [Asm.Instruction] -> [Asm.Instruction]
interleave a b =
  let aMain = extractMain a in
  let bMain = extractMain b in
  mainPrefix ++ concat (zipWith combineInstructions aMain bMain)
  where
    extractMain :: [Asm.Instruction] -> [Asm.Instruction]
    extractMain = dropWhile (\a -> "main" `notElem` Asm.labels a)

    mainPrefix :: [Asm.Instruction]
    mainPrefix = [Asm.Instruction ".globl" ["main"] [],
                  Asm.Instruction ".type" ["main", "@function"] []]

combineInstructions
  :: Asm.Instruction -> Asm.Instruction -> [Asm.Instruction]
combineInstructions a b =
  [a { Asm.labels = Asm.labels a `union` Asm.labels b },
   b { Asm.labels = [] }]

postfixNames :: String -> Asm.Program -> Asm.Program
postfixNames postfix prog = prog {
  Asm.funcs = map postfixFunction (Asm.funcs prog),
  Asm.readOnlyData = map postfixRod (Asm.readOnlyData prog)
} where
  postfixFunction :: Asm.Func -> Asm.Func
  postfixFunction f = f {
    Asm.name = Asm.name f ++ postfix,
    Asm.instructions = map
                       (postfixLabels . postfixCalls . postfixRodAccess)
                       (Asm.instructions f)
  }
  postfixRod :: Asm.ReadOnlyData -> Asm.ReadOnlyData
  postfixRod rod = rod {
    Asm.readOnlyLines = map postfixLabels (Asm.readOnlyLines rod)
  }
  postfixLabels :: Asm.Instruction -> Asm.Instruction
  postfixLabels i = i {
    Asm.labels = map (++ postfix) (Asm.labels i)
  }
  postfixCalls :: Asm.Instruction -> Asm.Instruction
  postfixCalls i
    | Asm.command i == "call" =
      i {Asm.arguments = map (++ postfix) (Asm.arguments i)}
    | otherwise = i
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

