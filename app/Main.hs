module Main where

import Lib
import System.Environment (getArgs, getProgName)
import Data.List (isInfixOf)

outputFilePath = "bin/output.s"

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName

  program <- case args of
    [path1, path2] | ".c" `isInfixOf` path1 && ".c" `isInfixOf` path2 ->
      interleaveCFiles path1 path2
    [path1, path2] | ".s" `isInfixOf` path1 && ".s" `isInfixOf` path2 ->
      interleaveAsmFiles path1 path2
    _ -> error $ "Usage: " ++ progName ++ " <ASM/C path> <ASM/C path>"

  putStrLn $ "Writing output to " ++ outputFilePath
  saveProgram outputFilePath program

