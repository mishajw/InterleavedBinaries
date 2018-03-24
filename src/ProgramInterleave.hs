module ProgramInterleave (
  simpleInterleave)
  where

import Data.List (union, partition)
import qualified Asm
import NameResolver (postfixNames)

simpleInterleave :: Asm.Program -> Asm.Program -> Asm.Program
simpleInterleave prog1 prog2 =
  -- Get the main and start functions
  -- We discard one of the start functions because they (should be) identical
  let prog1' = postfixNames "_1" prog1 in
  let prog2' = postfixNames "_2" prog2 in
  let (start1, funcs1) = getFunc "_start_1" (Asm.funcs prog1') in
  let (start2, funcs2) = getFunc "_start_2" (Asm.funcs prog2') in
  let interleavedStart = interleaveFunc start1 start2 in
  Asm.Program {
    Asm.funcs = setName "_start" interleavedStart : funcs1 ++ funcs2,
    Asm.readOnlyData = Asm.readOnlyData prog1' ++ Asm.readOnlyData prog2'
 }
  where
    getFunc :: String -> [Asm.Func] -> (Asm.Func, [Asm.Func])
    getFunc funcName fs =
      let ([start], others) = partition (\f -> Asm.name f == funcName) fs in
      (start, others)

    setName :: String -> Asm.Func -> Asm.Func
    setName name f = f {
      Asm.name = name,
      Asm.instructions = headIns {
        Asm.labels = name : Asm.labels headIns
      } : tailIns
    } where
      headIns = head $ Asm.instructions f
      tailIns = tail $ Asm.instructions f

    interleaveFunc :: Asm.Func -> Asm.Func -> Asm.Func
    interleaveFunc f1 f2 =
      Asm.Func {
        Asm.name = Asm.name f1,
        Asm.instructions =
          interleaveInstructions (Asm.instructions f1) (Asm.instructions f2)
      }

interleaveInstructions
  :: [Asm.Instruction] -> [Asm.Instruction] -> [Asm.Instruction]
interleaveInstructions = interleaveInstructions' 0 where
  interleaveInstructions'
    :: Int -> [Asm.Instruction] -> [Asm.Instruction] -> [Asm.Instruction]
  interleaveInstructions' balance (a : as) bs | balance >= 0 =
    let (tied, as') = takeTied (a : as) in
    tied ++ interleaveInstructions' (balance - length tied) as' bs
  interleaveInstructions' balance as (b : bs) =
    let (tied, bs') = takeTied (b : bs) in
    tied ++ interleaveInstructions' (balance + length tied) as bs'
  interleaveInstructions' _ [] bs = bs
  interleaveInstructions' _ as [] = as

  takeTied :: [Asm.Instruction] -> ([Asm.Instruction], [Asm.Instruction])
  takeTied (i : rest) | Asm.tiedToNext i =
    let (tiedTo, rest') = takeTied rest in (i : tiedTo, rest')
  takeTied (i : rest) = ([i], rest)
  takeTied [] = ([], [])

combineInstructions
  :: Asm.Instruction -> Asm.Instruction -> [Asm.Instruction]
combineInstructions a b =
  [a { Asm.labels = Asm.labels a `union` Asm.labels b },
   b { Asm.labels = [] }]

