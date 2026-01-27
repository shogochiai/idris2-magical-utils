# Playwright CLI では V8 カバレッジ収集できない

## 背景

PlaywrightRunner.idr は以下の4段階の間接層を経て V8 カバレッジを収集している:

```
Idris2 -> temp .js 文字列生成 -> ファイル書出 -> system("node /tmp/playwright_cov_*.js") -> JSON 読取 -> cleanup
```

この間接層を Playwright CLI で簡素化できないか検討した。

## 結論: CLI では不可

`page.coverage.startJSCoverage()` は内部的に CDP (Chrome DevTools Protocol) の `Profiler.startPreciseCoverage` を呼んでいる。これはセッションベースのプロファイラ制御であり、以下の順序で CDP コネクションを維持したまま複数コマンドを逐次発行する必要がある:

```
Profiler.startPreciseCoverage -> ページロード / JS 実行 -> Profiler.stopPreciseCoverage -> Profiler.takePreciseCoverage
```

Playwright CLI は本質的にステートレス (1コマンド = 1操作 = 終了) なので、このセッション制御はできない。`playwright open`, `playwright screenshot` 等の単発操作とは本質的に異なる。

Playwright CLI の公開コマンド一覧に `coverage` 系は存在しない。`run-code` や `eval` といったコマンドも存在しない。

## 実際に有効な簡素化

既に `lib/coverage.mjs` が Playwright のカバレッジ収集を実装済み。`PlaywrightRunner.idr` はこれとは別に毎回 Playwright スクリプトを文字列生成しており、ここに冗長性がある。

### 改善案: lib/coverage.mjs の直接呼び出し

```
現状:
  Idris2 -> temp .js 文字列生成 -> ファイル書出 -> node temp.js -> JSON 読取 -> cleanup

改善案:
  Idris2 -> system("node lib/coverage.mjs --js <path> --output <path> --timeout <ms>") -> JSON 読取
```

- temp ファイル生成・管理が不要になる
- Playwright の programmatic API (CDP セッション制御) はそのまま維持
- `bin/idris2-web-cov.mjs` が既に `lib/coverage.mjs` を呼んでいるので、同じインターフェースを Idris2 側からも使える

## AI エージェント連携のコンテキスト効率について

MCP 経由 vs CLI 経由のコンテキスト消費量比較 (8% vs 1.3%) は、screenshot / click / navigation 等の UI 操作系に当てはまる話。V8 カバレッジ収集は MCP でも CLI でもなく programmatic API を叩くスクリプト実行になるため、この数値比較は直接適用できない。
