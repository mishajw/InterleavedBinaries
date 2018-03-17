module Lib (
  interleavePrograms,
  interleaveCFiles,
  interleaveAsmFiles)
  where

import System.FilePath.Posix ((</>), (<.>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import qualified Asm
import qualified System.Exit as Exit

-- | Interleave two programs
interleavePrograms
  :: Asm.Program -- ^ The first program to interleave
  -> Asm.Program -- ^ The second program to interleave
  -> Asm.Program -- ^ The interleaved program
interleavePrograms a _ = a

-- | Interleave two .c files
-- Delagates to @interleavePrograms@
interleaveCFiles
  :: FilePath -- | The path of the first .c file
  -> FilePath -- | The path of the second .c file
  -> IO Asm.Program -- | The interleaved program
interleaveCFiles path1 path2 = withSystemTempDirectory "/tmp/" run where
  -- | Run on @cFileToAsmFile@ both paths and pass to @interleavePrograms@
  run :: FilePath -> IO Asm.Program
  run directory = do
    asmFile1 <- cFileToAsmFile path1 directory 1
    asmFile2 <- cFileToAsmFile path2 directory 2
    interleaveAsmFiles asmFile1 asmFile2

-- | Interleave two ASM (.s) files
-- Delagates to @interleavePrograms@
interleaveAsmFiles
  :: FilePath -- ^ The path of the first ASM file
  -> FilePath -- ^ The path of the second ASM file
  -> IO Asm.Program -- ^ the interleaved program
interleaveAsmFiles path1 path2 =
  interleavePrograms <$> asmFileToProgram path1 <*> asmFileToProgram path2

-- | Change a .c file to an ASM (.s) file
cFileToAsmFile
  :: FilePath -- ^ The path of the .c file
  -> FilePath -- ^ The output path for the ASM file
  -> Int -- ^ Index of the program we're creating
  -> IO FilePath -- ^ The path of the created ASM file
cFileToAsmFile inputPath directory programIndex = do
  let asmFile = directory </> ("asm" ++ show programIndex) <.> "s"
  (exitCode, _, _) <- readProcessWithExitCode "gcc" [
    "-O0",
    "-S", inputPath,
    "-o", asmFile] ""

  case exitCode of
    Exit.ExitSuccess ->
      return asmFile
    Exit.ExitFailure n ->
      error $ "Bad return code when generating ASM: " ++ show n

-- | Convert an ASM file to a @Asm.Program@
asmFileToProgram
  :: FilePath -- ^ The path of the ASM file
  -> IO Asm.Program -- ^ The program
asmFileToProgram inputPath = Asm.stringToProgram <$> readFile inputPath

