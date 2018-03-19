module InstructionInterleave (interleave)
  where

import Data.List (union)
import qualified Asm

interleave :: [Asm.Instruction] -> [Asm.Instruction] -> [Asm.Instruction]
interleave a b =
  let aMain = extractMain a in
  let bMain = extractMain b in
  mainPrefix ++ concat (zipWith combineInstructions aMain bMain)
  where
    combineInstructions
      :: Asm.Instruction -> Asm.Instruction -> [Asm.Instruction]
    combineInstructions a b =
      [a { Asm.labels = Asm.labels a `union` Asm.labels b },
       b { Asm.labels = [] }]

    extractMain :: [Asm.Instruction] -> [Asm.Instruction]
    extractMain = dropWhile (\a -> "main" `notElem` Asm.labels a)

    mainPrefix :: [Asm.Instruction]
    mainPrefix = [Asm.Instruction ".globl" ["main"] [],
                  Asm.Instruction ".type" ["main", "@function"] []]

