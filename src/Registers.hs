module Registers (
    Reg (..),
    Size (..),
    SizedReg (..),
    allRegisters)
  where

import Data.Maybe (catMaybes)
import Text.Read
import Text.Regex (mkRegex, matchRegex, matchRegexAll)

validRegisterNames :: [String]
validRegisterNames =
  ["ax", "bx", "cx", "dx", "bp", "sp", "di", "si",
   "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]

allRegisters :: [Reg]
allRegisters = map read validRegisterNames

-- | Reference to a register (unsized)
newtype Reg = Reg { name :: String } deriving Eq

-- | The size of a register
data Size = Size64 | Size32 deriving Eq

-- | A register with a set size
data SizedReg = SizedReg { reg :: Reg, size :: Size } deriving Eq

instance Show Reg where
  show (Reg name) = name

instance Read Reg where
  readsPrec _ s = [(Reg s, "") | s `elem` validRegisterNames]

instance Ord Reg where
  compare r1 r2 = compare (name r1) (name r2)

instance Show SizedReg where
  show (SizedReg reg size) = case getNumberedRegister reg of
    Just _ -> case size of
      Size64 -> name reg
      Size32 -> name reg ++ "d"
    Nothing -> case size of
      Size64 -> "r" ++ name reg
      Size32 -> "e" ++ name reg

instance Read SizedReg where
  readsPrec _ s = map (\r -> (r, "")) $
                  catMaybes [tryR64, tryR32, tryMain] where
    tryR64 :: Maybe SizedReg
    tryR64 = case readMaybe s :: Maybe Reg of
      Just r -> Just $ SizedReg r Size64
      Nothing -> Nothing
    tryR32 = case readMaybe (init s) :: Maybe Reg of
      Just r -> Just $ SizedReg r Size32
      Nothing -> Nothing
    tryMain :: Maybe SizedReg
    tryMain = case s of
      ['r', c1, c2] -> case readMaybe [c1, c2] :: Maybe Reg of
        Just r -> Just $ SizedReg r Size64
        Nothing -> Nothing
      ['e', c1, c2] -> case readMaybe [c1, c2] :: Maybe Reg of
        Just r -> Just $ SizedReg r Size32
        Nothing -> Nothing

getNumberedRegister :: Reg -> Maybe Int
getNumberedRegister (Reg s) =
  case matchRegex (mkRegex "r([0-9]+)") s of
    Just [numString] -> readMaybe numString
    Nothing -> Nothing

