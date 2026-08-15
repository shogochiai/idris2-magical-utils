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

## 訂正 (2026-08-15) — 「どのゲートにも載っていない」は言い過ぎだった

直前の節で「ERC-7546 の実装一式は一度も品質ゲートに載っていない」と書いた。
**SemanticAudit については偽である。** luci は SPEC.toml を2通りに探している:

| 経路 | 探し方 | 入れ子を拾うか |
|---|---|---|
| `[[lazy]]` / dump-s (`InstanceConfig.idr:109`) | `projectDir/target/SPEC.toml` のリテラル連結 | 拾わない |
| `ParityDs.idr:292` | `find pkgs -maxdepth 2 -name SPEC.toml` | 拾わない |
| `SemanticAudit.idr:77` | `find <srcPath> -name 'SPEC.toml'` (**再帰**) | **拾う** |

したがって正確な主張はこうである: **入れ子の SPEC.toml は dump-s と ParityDs
からは見えず、SemanticAudit からは見える。** 3つの消費者のうち2つで不可視、
というのが実態で、「どれからも不可視」ではない。

**なぜ間違えたか**: CLAUDE.md の [[lazy]] 登録義務が「dump-s に出ない / AGA
gate の対象外 / SemanticAudit の対象外」を**1つの帰結として**並べているので、
1つ (dump-s の 0件) を測って残り2つを推論した。**測ったのは1つだけだった。**
これは、まさに「好みではなく測定で決めた」と書いた同じコミットの中で起きている。

**修理方針への影響**: 入れ子ファイルには消費者が在る (SemanticAudit) ので
「参照されていない孤児だから自由に動かせる」という前提は使えない。ただし
SemanticAudit の探索は再帰なので、**root に移しても引き続き見つかる** —
移動そのものは安全である。壊れるとしたら入れ子のパスを literal で持つ何かだが、
それは grep で1件も無い (この doc 自身を除く)。

## 撤回 (2026-08-15) — dump-s は入れ子を集約する。前2節の結論は誤り

```
$ luci dump-s --pkg pkgs/Idris2Subcontract | grep -c 'OU_'
44
## Module: .../src/Subcontract/Standards/ERC7546/OptimisticUpgrader/...
- [OU_PROP_001] Only proposer can create proposals
```

**集約されている。** 「0 distinct REQ ids」は、このパッケージの
`[definitions].prefix` が **`OU`** であって `REQ` ではないのに、私が
`grep -oE 'REQ_[A-Za-z0-9_]+'` で数えたことによる。出力には最初から
`OU_PROP_001` 等が 44 箇所あった。

**対照は効かなかった。** `Idris2AndroidCoverage → 5 REQ ids` を対照に置いたが、
これが示すのは「計器は REQ_ 接頭辞のパッケージで動く」ことだけである。
2つのパッケージ間で違っていた変数は**集約の有無ではなく接頭辞**であり、
私は対照を**間違った軸**に置いた。対照を置いたこと自体が、置いた向きの
正しさを保証しない。

### したがって撤回するもの

- 「dump-s は入れ子の SPEC.toml を集約しない」— **偽**
- 「修理は測定で決まった: package-root の SPEC.toml が要る」— **未確立**
- 「ERC-7546 実装一式はゲートに載っていない / silent success」— **偽**。
  OptimisticUpgrader の 22 spec は dump-s から見えている。
  (その前の「SemanticAudit だけは見える」という訂正も、前提が崩れたので無効。)

### 生き残るもの

`pkgs/Idris2Subcontract/SPEC.toml` が**存在しない**のは事実で、architect が
慣例どおり書いた `grep -q 'REQ_…' pkgs/Idris2Subcontract/SPEC.toml` が
`No such file` で exit 2 になり 10 件が通らなかったのも事実である。
**4枚目の壁の症状は正しく、原因の説明が間違っていた。**

### 本当の選択肢

パッケージを再構成する必要は無い。安いのは**プロンプト側**である —
新しい spec をどのファイルに置くかを明示し、prefix を既存語彙に揃える
(`OU` の隣に `REQ_` を混ぜるのか、`SC_FACADE_*` のような別 prefix にするのかは
未決定。gap-ledger のクラスタ化キーは先頭3トークンなので、ここは適当に決めない)。
