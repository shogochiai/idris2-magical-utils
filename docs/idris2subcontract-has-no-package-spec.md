# Idris2Subcontract は [[lazy]] 登録済みだが package-root SPEC.toml を持たない

2026-08-15 実測。`luci --subagent` がこのリポジトリで初めて動いたときに露出した。

`LUCICONF.toml:91-93` は `target = "pkgs/Idris2Subcontract"` / `family = "evm"` を
登録している。しかし **`pkgs/Idris2Subcontract/SPEC.toml` は存在しない**。
仕様はもっと深い階層に3つ散っている:

```
examples/TokenPJ/src/Main/SPEC.toml
src/Subcontract/Standards/ERC7546/Inception/SPEC.toml
src/Subcontract/Standards/ERC7546/OptimisticUpgrader/SPEC.toml
```

**兄弟パッケージは全部 package-root に持っている** (Idris2AndroidCoverage /
Idris2LspRedox / idris2fficoverage / idris2ffiintegration)。つまりこの
パッケージだけが慣例から外れている。

## 何が壊れるか

architect が受け入れチェックを合成するとき、慣例どおり
`grep -q 'REQ_…' pkgs/Idris2Subcontract/SPEC.toml` を書く。ファイルが無いので
grep は **exit 2** を返し、ハーネスはそれを `UsageExitTwo` (malformed) として
**除外**する。結果 `0 of 10 passed, 5 check excluded` になり、executor が何を
書いたかに関係なく run は失敗する。

**副次的な発見 — ラベルが原因を取り違えている**: `UsageExitTwo` の説明文は
「a grep/sed/awk usage error, e.g. a pattern cut onto the next line」と書くが、
実際の stderr は `No such file or directory` だった。grep は usage エラーでも
**ファイル不在でも** 2 を返す。片方だけを名指しするラベルは、読者を存在しない
原因 (パターンの折り返し) に向かわせる。

## したがって

- このパッケージに対する開発要求を書くときは、**SPEC.toml の場所を明示する**
  (プロンプトは API である — 慣例を仮定した私の要求がこの run を落とした)。
- 恒久対処は package-root に `SPEC.toml` を置いて `[[lazy]]` の前提に揃えるか、
  `[[lazy]]` 側が入れ子の SPEC.toml を集約できるようにするかのどちらか。
  **どちらが正しいかはまだ測っていない** (入れ子 SPEC.toml を読む経路が既に
  あるのかを確認していない)。
