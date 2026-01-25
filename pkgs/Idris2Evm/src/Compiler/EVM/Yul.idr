||| EVM Backend Entry Point
||| Idris2 Codegen implementation for EVM (via Yul)
module Compiler.EVM.Yul

import Compiler.EVM.YulIR
import Compiler.EVM.Codegen
import Compiler.EVM.Memory

import Core.Context
import Core.Core
import Core.Directory
import Compiler.Common
import Compiler.ANF
import Idris.Syntax

import Data.List
import Data.String
import System.File

%default covering

-- =============================================================================
-- Backend Implementation
-- =============================================================================

||| Compile Idris2 expression to Yul
compileToYul : Ref Ctxt Defs ->
               Ref Syn SyntaxInfo ->
               (tmpDir : String) ->
               (outputDir : String) ->
               ClosedTerm ->
               (outfile : String) ->
               Core (Maybe String)
compileToYul c s tmpDir outputDir tm outfile = do
  -- Get compile data at ANF phase
  cdata <- getCompileData False ANF tm

  -- Extract ANF definitions
  let defs = anf cdata

  -- Debug: dump ANF to file
  let anfDumpPath = outputDir ++ "/" ++ outfile ++ ".anf"
  let anfContent = unlines $ map (\(n, d) => show n ++ " = " ++ show d) defs
  _ <- coreLift $ writeFile anfDumpPath anfContent

  -- Generate Yul object
  yulObj <- generateYul "Contract" defs

  -- Write to output file
  let yulPath = outputDir ++ "/" ++ outfile ++ ".yul"
  writeYulFile yulPath yulObj

  pure (Just yulPath)

||| Execute Idris2 expression (not supported for EVM)
executeYul : Ref Ctxt Defs ->
             Ref Syn SyntaxInfo ->
             (tmpDir : String) ->
             ClosedTerm ->
             Core ()
executeYul c s tmpDir tm = do
  coreLift $ putStrLn "EVM backend does not support direct execution."
  coreLift $ putStrLn "Use 'solc --strict-assembly' to compile the generated Yul."

-- =============================================================================
-- Codegen Export
-- =============================================================================

||| EVM/Yul code generator
export
codegenEVM : Codegen
codegenEVM = MkCG compileToYul executeYul Nothing Nothing

||| Alternative name
export
codegenYul : Codegen
codegenYul = codegenEVM
