# Build System Contract Issues in WasmBuilder

## 概要

WasmBuilder でテストビルドとプロダクションビルドを統一的に扱う際に発生する
**Integration/Build-System Contract Issues** とその解決パターン。

## 問題分類

| 問題 | 分類 | 症状 |
|------|------|------|
| 相対パスシンボリックリンク | **環境結合** | temp dirから元ファイルが見つからない |
| test/prod で異なる検索パス | **ビルド設定ドリフト** | "No C file generated" 偽陽性 |
| Main.idr と canister_entry.c の名前不整合 | **コード生成契約ドリフト** | `Main_runTests` import not found |

## 修正箇所 (WasmBuilder.idr)

### 1. setupTestBuild: 絶対パス変換

```idris
setupTestBuild projectDir originalIpkg customTestPath = do
  -- Convert projectDir to absolute path (needed for symlinks to work from temp dir)
  (_, absProjectDir', _) <- executeCommand $ "cd " ++ projectDir ++ " && pwd"
  let absProjectDir = trim absProjectDir'
  -- ... use absProjectDir for symlinks
```

**理由**: `ln -sf ./src/Foo.idr /tmp/xxx/src/Foo.idr` は temp dir から解決できない

### 2. compileWithIpkg: ビルド成果物検索パス

```idris
compileWithIpkg opts' ipkg = do
  let ipkgDir = dirname ipkg
  let buildSearchDir = if ipkgDir /= opts'.projectDir && not (null ipkgDir)
        then ipkgDir ++ "/build"
        else opts'.projectDir ++ "/build"
  -- ... search for C file in buildSearchDir
```

**理由**: テストビルドの ipkg は `/tmp/idris2-wasm-test-xxx/test_build.ipkg` にあり、
成果物は `/tmp/idris2-wasm-test-xxx/build/` に出力される（`opts'.projectDir/build/` ではない）

### 3. generateTestMainContent: 名前衝突回避

```idris
generateTestMainContent testModuleName = unlines
  [ -- ...
  , "import " ++ testModuleName  -- NOT "import public" (causes name clash)
  , -- ...
  , "runTests : IO (Int, Int)"
  , "runTests = do"
  , "  let (passed, failed) = " ++ testModuleName ++ ".runAllTests"
  , "  pure (cast passed, cast failed)"  -- Explicit type conversion
  , -- ...
  , "    _ <- Main.runTests"  -- Explicit module prefix (avoids ambiguity)
  ]
```

**理由**:
- `import public` だと `Tests.AllTests.runTests` が Main スコープに入り衝突
- `(Nat, Nat)` → `IO (Int, Int)` は暗黙変換できない
- `runTests` だけだと `Main.runTests` と `Tests.AllTests.runTests` が曖昧

### 4. generateCanisterEntryC: モジュールプレフィックス統一

```idris
let modulePrefix = "Main"
let entryC = generateCanisterEntryC modulePrefix effectiveExports didMethods typeDefs
```

**理由**: テストビルドでも `Main_*` 関数名を使用することで canister_entry.c との整合性を保証

## 設計原則

1. **Single Source of Truth**: エントリ関数名は一箇所で定義
2. **パラメータ化バリアント**: test/prod は別パイプラインではなくパラメータの違い
3. **絶対パス優先**: シンボリックリンクは常に絶対パスで作成
4. **成果物場所の導出**: ipkg の場所からビルド成果物の場所を導出

## 参考

問題分類は Codex (gpt-5.1-codex-max) 分析に基づく：

> These are build/integration tooling issues rather than IO-monad boundary issues.
> They're mismatches between the build system's assumptions and generated artifacts,
> i.e., Integration/Build Pipeline problems.
