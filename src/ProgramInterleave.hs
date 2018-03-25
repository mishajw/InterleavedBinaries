module ProgramInterleave (
  interleave,
  simpleInterleave)
  where

import Debug.Trace

import Control.Monad.State
import Data.List (union, partition, find, intercalate)
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

data Position = Position { func :: String, index :: Int, tied :: Bool } deriving Show
data CreateState = CreateState {
  balance :: Int,
  stackA :: [Position],
  stackB :: [Position],
  writeFunc :: String,
  created :: Asm.Program
}
interleave :: Asm.Program -> Asm.Program -> Asm.Program
interleave prog0 prog1 =
  let pos0 = Position "_start_0" 0 False in
  let pos1 = Position "_start_1" 0 False in
  let rods = Asm.readOnlyData prog0 ++ Asm.readOnlyData prog1 in
  let prog = Asm.Program [Asm.Func "_start" []] rods in
  let
    (_, finalState) =
      runState interleave' (CreateState 0 [pos0] [pos1] "_start" prog) in
  addFuncLabels $ created finalState where

  interleave' :: State CreateState ()
  interleave' = do
    (pos, isA) <- getCurIns

    case (getNextPos pos isA, getBranch pos isA) of
      -- Add the next position to the stack if it exists
      (Just nextPos, Just branchTo) -> do
        -- Put the next branch and return address on the stack
        putOnStack nextPos isA
        putOnStack branchTo isA
        -- Create the function and the jump to the function
        prog <- created <$> get
        let jmpIns = getIns pos isA
        funcName <- getFuncName
        oldFuncName <- writeFunc <$> get
        let progWithJmp = Asm.insertEndFuncton
                      oldFuncName
                      [jmpIns {Asm.arguments = [funcName]}]
                      prog
        let progWithNewFunc = prog {Asm.funcs =
          Asm.Func funcName [] : Asm.funcs progWithJmp
        }
        state <- get
        put $ state {writeFunc = funcName, created = progWithNewFunc}
        -- Create the child function
        interleave'
        -- Reset the function we're writing to
        state <- get
        put $ state {writeFunc = oldFuncName}

      (Just nextPos, Nothing) -> do
        -- Update the program with the new instruction
        oldState <- get
        let oldProg = created oldState
        let wr = writeFunc oldState
        let ins = getIns pos isA
        put $ oldState {created = Asm.insertEndFuncton wr [ins] oldProg}

        putOnStack nextPos isA
      _ -> do
        oldState <- get
        let oldProg = created oldState
        let wr = writeFunc oldState
        let ins = getIns pos isA
        put $ oldState {created = Asm.insertEndFuncton wr [ins] oldProg}

        trace (show pos) return ()

    as <- stackA <$> get
    bs <- stackB <$> get
    unless (null as && null bs) interleave'

  getCurIns :: State CreateState (Position, Bool)
  getCurIns = do
    oldState <- get
    let i = balance oldState
    let as = stackA oldState
    let bs = stackB oldState
    if null bs || (i >= 0 && not (null as))
    then do
      put $ oldState {balance = i - 1, stackA = tail as}
      return (head as, True)
    else do
      put $ oldState {balance = i + 1, stackB = tail bs}
      return (head bs, False)

  putOnStack
    :: Position -> Bool -> State CreateState ()
  putOnStack pos isA = do
    oldState <- get
    let as = stackA oldState
    let bs = stackB oldState
    if isA
    then put $ oldState {stackA = pos : as}
    else put $ oldState {stackB = pos : bs}

  getFuncName :: State CreateState String
  getFuncName = do
    as <- stackA <$> get
    bs <- stackB <$> get
    let
      positions = case (as, bs) of
        (a : as, b : bs) -> [a, b]
        (a : as, []) -> [a]
        ([], b : bs) -> [b]
        ([], []) -> []
    let shownPositions = map
                         (\p -> func p ++ "_" ++ show (index p))
                         positions
    return $ intercalate "_" shownPositions

  getNextPos :: Position -> Bool -> Maybe Position
  getNextPos pos isA =
    let Just posFunc = getFunc (func pos) (if isA then prog0 else prog1) in
    let numIns = length $ Asm.instructions posFunc in
    if numIns > index pos + 1
    then Just (pos {index = index pos + 1})
    else Nothing

  getFunc :: String -> Asm.Program -> Maybe Asm.Func
  getFunc name prog = find (\f -> Asm.name f == name) (Asm.funcs prog)

  getBranch :: Position -> Bool -> Maybe Position
  getBranch pos isA =
    let ins = getIns pos isA in
    if Asm.command ins == "jmp"
    then Just $ Position (head (Asm.arguments ins)) 0 False
    else Nothing

  getIns :: Position -> Bool -> Asm.Instruction
  getIns pos isA =
    let prog = if isA then prog0 else prog1 in
    let Just posFunc = getFunc (func pos) prog in
    Asm.instructions posFunc !! index pos

  addFuncLabels :: Asm.Program -> Asm.Program
  addFuncLabels prog = prog { Asm.funcs = map mapFunc (Asm.funcs prog) } where
    mapFunc :: Asm.Func -> Asm.Func
    mapFunc func | not (null $ Asm.instructions func)= func {
      Asm.instructions = headIns {
        Asm.labels = Asm.name func : Asm.labels headIns
      } : tailIns
    } where
      headIns = head (Asm.instructions func)
      tailIns = tail (Asm.instructions func)
    mapFunc func = func

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

