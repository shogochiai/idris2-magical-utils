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

## 決着 (2026-08-15、対照付き)

「入れ子の SPEC.toml を読む経路が既にあるか」を測った。**無い。**

```
luci dump-s --pkg pkgs/Idris2AndroidCoverage  ->  5 distinct REQ ids   (対照)
luci dump-s --pkg pkgs/Idris2Subcontract      ->  0 distinct REQ ids
```

対照を並べたのは、0 が「集約されない」なのか「私のコマンドが間違い」なのかを
区別するため。root に SPEC.toml を持つ兄弟が 5 件返すので、計器は動いており
**0 は Idris2Subcontract についての事実**である。

したがって修理は好みではなく測定で決まる: **package-root に `SPEC.toml` が要る**。

## これは facade タスクより大きい

CLAUDE.md の [[lazy]] 登録義務はこう書いている — 未登録のパッケージは
`dump-s` に出ず、AGA Loop の quality gate の対象外になり、SemanticAudit の
対象外になる。**Idris2Subcontract は登録済みなのに仕様を1件も供給していない**
ので、登録の効果としては未登録と同じである。

つまり **ERC-7546 の実装一式 — proxy / dictionary / slots / upgrade validation —
は、登録済みに見えて一度も品質ゲートに載っていない**。これは「登録した」と
「測られている」が別物である例で、外からは区別がつかない (登録は成功しており、
エラーは出ない)。

ここに facade を足せば、facade もまた測られないまま増える。**SPEC.toml が
facade の前提である。**
