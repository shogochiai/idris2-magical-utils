||| The `react-native` code generator.
|||
||| This is a thin derivative of the standard ES (EcmaScript) backend
||| (`Compiler.ES.Codegen`). React Native runs on a JavaScript engine
||| (Hermes / JSC), so the existing ES output runs there almost verbatim.
|||
||| The only meaningful differences from the stock `javascript` backend are:
|||
||| 1. The `ccTypes` list (the backends `%foreign` declarations are searched
|||    against) is led by "react-native", so `%foreign "react-native:..."`
|||    implementations win, falling back to "browser"/"javascript".
||| 2. The output is a plain ES module (no HTML wrapper) suitable for the
|||    Metro bundler to consume.
|||
||| We deliberately reuse the existing `Javascript` `CG` tag rather than
||| adding a new constructor to `Core.Options.CG`. The `cg` argument only
||| influences `getDirectives cg` (the `--directive` namespace); the actual
||| code shape is driven by `ccTypes`. Reusing `Javascript` keeps this an
||| out-of-tree plugin (registered via `mainWithCodegens`) with zero changes
||| to the forked compiler — the same non-invasive pattern idris2-evm uses.
module Compiler.ReactNative.Codegen

import Compiler.ES.Codegen

import Compiler.Common

import Libraries.Utils.Path

import Idris.Env
import Idris.Syntax

import Data.String

import Core.Context
import Core.Core
import Core.Directory

%default covering

||| The backends searched (in order) for `%foreign` implementations.
||| React-native-specific FFI wins; otherwise we fall back to the standard
||| browser / javascript implementations shared with idris2-dom-mvc.
rnCCTypes : List String
rnCCTypes = ["react-native", "browser", "javascript"]

||| Compile a TT expression to React-Native-flavoured JavaScript.
compileToRN :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  ClosedTerm -> Core String
compileToRN c s tm = compileToES c s Javascript tm rnCCTypes

||| `compileExpr` interface implementation for the react-native backend.
||| Emits a plain ES module (no HTML wrapper) for Metro / Hermes to consume.
compileExpr :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  (tmpDir : String) ->
  (outputDir : String) ->
  ClosedTerm ->
  (outfile : String) ->
  Core (Maybe String)
compileExpr c s tmpDir outputDir tm outfile =
  do let out = outputDir </> outfile
     es <- compileToRN c s tm
     Core.writeFile out es
     pure (Just out)

||| The react-native backend cannot execute directly; it only emits a JS
||| bundle for Metro / Hermes to run.
executeExpr :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  (tmpDir : String) -> ClosedTerm -> Core ()
executeExpr c s tmpDir tm =
  throw $ InternalError "react-native backend only compiles; run the emitted bundle under React Native / Hermes"

||| Codegen record for the react-native backend.
export
codegenReactNative : Codegen
codegenReactNative = MkCG compileExpr executeExpr Nothing Nothing
