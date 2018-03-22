module Lib (
  interleavePrograms,
  interleaveCFiles,
  interleaveAsmFiles,
  cFileToProgram,
  asmFileToProgram,
  saveProgram)
  where

import AsmTransformer (transform)
import InstructionInterleave (interleave)
import System.FilePath.Posix ((</>), (<.>), dropExtension, takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import qualified Asm
import qualified System.Exit as Exit

-- | Interleave two programs
interleavePrograms
  :: Asm.Program -- ^ The first program to interleave
  -> Asm.Program -- ^ The second program to interleave
  -> Asm.Program -- ^ The interleaved program
interleavePrograms prog0 prog1 =
  let prog0Trans = transform 0 prog0 in
  let prog1Trans = transform 1 prog1 in
  prog1Trans

-- | Interleave two .c files
-- Delagates to @interleavePrograms@
interleaveCFiles
  :: FilePath -- ^ The path of the first .c file
  -> FilePath -- ^ The path of the second .c file
  -> IO Asm.Program -- ^ The interleaved program
interleaveCFiles path1 path2 =
  -- | Run on @cFileToAsmFile@ both paths and pass to @interleavePrograms@
    interleavePrograms <$> cFileToProgram path1 <*> cFileToProgram path2

-- | Interleave two ASM (.s) files
-- Delagates to @interleavePrograms@
interleaveAsmFiles
  :: FilePath -- ^ The path of the first ASM file
  -> FilePath -- ^ The path of the second ASM file
  -> IO Asm.Program -- ^ the interleaved program
interleaveAsmFiles path1 path2 =
  interleavePrograms <$> asmFileToProgram path1 <*> asmFileToProgram path2

-- | Load an @Asm.Program@ from a `.c` file
cFileToProgram
  :: FilePath -- ^ The path of the `.c` file
  -> IO Asm.Program -- ^ The program read from the file
cFileToProgram cPath = withSystemTempDirectory "/tmp/" run where
  run :: FilePath -> IO Asm.Program
  run directory = do
    -- Create path for ASM file
    let fileName = dropExtension $ takeFileName cPath
    let asmPath = directory </> fileName <.> "a"
    -- Create the ASM file
    cFileToAsmFile cPath asmPath
    -- Create a program from the ASM file
    asmFileToProgram asmPath

-- | Convert an ASM file to a @Asm.Program@
asmFileToProgram
  :: FilePath -- ^ The path of the ASM file
  -> IO Asm.Program -- ^ The program
asmFileToProgram inputPath = Asm.stringToProgram <$> readFile inputPath

-- | Save a program to an ASM file
saveProgram
  :: FilePath -- ^ Where to save the file
  -> Asm.Program -- ^ The program to save
  -> IO ()
saveProgram path program = writeFile path $ Asm.programToString program

-- | Change a .c file to an ASM (.s) file
cFileToAsmFile
  :: FilePath -- ^ The path of the .c file
  -> FilePath -- ^ The output file path for the ASM file
  -> IO ()
cFileToAsmFile inputPath outputPath = do
  -- let asmFile = directory </> ("asm" ++ show programIndex) <.> "s"
  (exitCode, _, _) <- readProcessWithExitCode "gcc" [
    "-O0", "-fno-asynchronous-unwind-tables",
    "-S", inputPath,
    "-o", outputPath] ""

  case exitCode of
    Exit.ExitSuccess ->
      return ()
    Exit.ExitFailure n ->
      error $ "Bad return code when generating ASM: " ++ show n

