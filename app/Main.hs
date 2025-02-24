{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import AbstractOpCode ()
import Control.Exception
import Control.Monad (when)
import GHC.IO.Handle (hSetBuffering)
import GHC.IO.Handle.FD (stdin)
import GHC.IO.Handle.Types (BufferMode (NoBuffering))
import Machine
import RiscV (generateAssembly)
import SimpleParser qualified as SP
import System.Console.CmdArgs
import System.Exit (exitFailure)
import System.IO.Error

data CompilerOpts = CompilerOpts
  { input :: FilePath,
    output :: Maybe FilePath,
    intermediate :: Bool,
    verbose :: Bool
  }
  deriving (Data, Typeable, Show)

defaultOpts :: CompilerOpts
defaultOpts =
  CompilerOpts
    { input = "-" &= argPos 0 &= typ "FILE",
      output = def &= typFile &= help "Output file (defaults to stdout)",
      intermediate = def &= help "output abstract machine code",
      verbose = def &= help "Verbose output"
    }
    &= summary "Vector Compiler 0.1.0.0"
    &= program "V"
    &= help "Compiles .v source files to riscv assembly"

readStdinSafe :: IO String
readStdinSafe = do
  hSetBuffering stdin NoBuffering
  handle handleIOError $ do
    getContents
  where
    handleIOError e
      | isEOFError e = return ""
      | otherwise = ioError e

getInputContent :: FilePath -> IO String
getInputContent "-" = readStdinSafe
getInputContent filepath = readFile filepath

writeOutput :: Maybe FilePath -> String -> IO ()
writeOutput Nothing output = putStr output
writeOutput (Just filepath) output = writeFile filepath output

main :: IO ()
main = do
  opts <- cmdArgs defaultOpts

  when (verbose opts) $ do
    putStrLn $ "Input file: " ++ input opts
    putStrLn $ "Output file: " ++ show (output opts)
    putStrLn $ "Verbose mode: " ++ show (verbose opts)

  content <- getInputContent (input opts)

  case SP.parse content of
    Left err -> do
      putStrLn "Parse error:"
      putStrLn err
      exitFailure
    Right ast -> do
      when (verbose opts) $
        putStrLn "Parsing successful, generating intermediate representation"
      let inter = runCompiler (genProgramm ast)
      if intermediate opts
        then do
          writeOutput (output opts) $ concatMap (\x -> show x ++ "\n") inter
        else do
          when (verbose opts) $
            putStrLn "Generating intermediate representation successful, generating assembly"
          let assembly = generateAssembly inter
          writeOutput (output opts) assembly
      when (verbose opts) $
        putStrLn "Compilation completed successfully"
