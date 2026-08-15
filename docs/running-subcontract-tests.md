# Idris2Subcontract のテストを実際に走らせる

`idris2 --find-ipkg --exec main <test file>` は**落ちる**:

```
Exception: attempt to reference unbound identifier blodwen-enter-test
```

しかも **rc=0 を返す** — 状態だけ見れば合格に見える。今夜2回これに時間を取られた。

## 原因

テストランナーは `System.Coverage.enterTest` を呼び、これは Chez の
`blodwen-enter-test` に落ちる。この記号は fork が**path-hit 記録の preamble の
一部として**出力するもので (`src/Compiler/Scheme/Chez.idr:524`)、**計装が有効な
ときにしか出ない**。

## 効く手順

```bash
export IDRIS2_BIN="$HOME/code/idrislang-idris2/build/exec/idris2"
HASH=<現在の pack collection hash>
IPP=$(ls -d "$HOME"/.local/state/pack/install/$HASH/*/*/idris2-0.8.0 | tr '\n' ':')
export IDRIS2_PACKAGE_PATH="${IPP}${HOME}/.local/state/pack/install/$HASH/idris2/idris2-0.8.0:${HOME}/.idris2/idris2-0.8.0"

cd pkgs/Idris2Subcontract
"$IDRIS2_BIN" --dumppathshits /tmp/hits.txt --find-ipkg --exec main \
  src/Subcontract/Core/Tests/AllTests.idr
```

**`--dumppathshits <file>` が要る。** これが preamble を出し、`enterTest` が解決する。
`IDRIS2_PACKAGE_PATH` の2根も要る (`carl` 機で実測、`build-apk.sh:28-40` と同型)。

## 実測結果 (2026-08-16)

```
src/Subcontract/Core/Tests/AllTests.idr
  [PASS] SDR_PLAN_001..006   (dry-run planner)
  Results: 42 passed, 0 failed

src/Subcontract/Standards/ERC7546/OptimisticUpgrader/Tests/AllTests.idr
  [PASS] OU_TEST_001, OU_TEST_003
  [WARNING] 22 pending test(s): declared but not yet verified
  Results: 3 passed, 0 failed, 22 pending
```

**22 pending が警告として出るのが正しい状態である** — それらの spec は宣言済みで
本体が無い。以前は「テスト有り」に見えていた。

## 注意

計装フラグを付けずに走らせると、例外を出しながら **rc=0** で終わる。
**状態コードだけで合否を判定してはならない** — 出力に `Results:` 行が
在ることを確かめること。
