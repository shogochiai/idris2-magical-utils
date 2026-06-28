//! revm-run — a fast, in-process drop-in for `idris2-evm-run --trace` in the EVM
//! coverage pipeline.
//!
//! Contract (mirrors idris2-evm-run so EvmCoverage.Runtime.parseProfileFlushCounters
//! parses our stdout unchanged):
//!
//!   revm-run --trace <bytecode-file> --gas <N>
//!     <bytecode-file> : runtime bytecode as trimmed hex (with or without 0x)
//!     --gas <N>       : gas limit (decimal)
//!
//! It deploys the runtime bytecode at a fixed address, calls it with empty
//! calldata under the given gas limit, and prints the captured LOG events in the
//! exact textual shape idris2-evm-run uses:
//!
//!   === Logs (K events) ===
//!   Log #i:
//!     Topics: T
//!       [j] 0x<64-hex topic>
//!     Data: B bytes
//!       0x<hex data>
//!   Result: SUCCESS|REVERT|... (gas used: G)
//!
//! Coverage is self-reported by the bytecode via a ProfileFlush LOG1 event, so
//! revm (the engine behind foundry/anvil/reth) surfaces it just like the
//! pure-Idris interpreter — only faster and more EIP-accurate.

use std::env;
use std::fs;
use std::process;

use revm::{
    db::{CacheDB, EmptyDB},
    inspector_handle_register,
    interpreter::Interpreter,
    primitives::{
        AccountInfo, Address, Bytecode, Bytes, ExecutionResult, Log, Output, TxKind, U256,
    },
    Database, EvmContext, Evm, Inspector,
};

const CALLEE: [u8; 20] = [0x10; 20];
const CALLER: [u8; 20] = [0x20; 20];

/// Captures every LOG event AS IT EXECUTES, before any revert rolls it back.
///
/// Path-coverage markers are emitted by the contract via `log1(0,0,<path-id>)`.
/// A deployed contract is pure dispatch: the handler runs and fires its markers,
/// but if that handler then reverts (e.g. `vote` on empty state) the EVM rolls
/// back ExecutionResult::logs() per spec — so the markers would be lost. The
/// inspector's `log` hook fires at opcode-execution time, capturing the markers
/// regardless of the eventual revert. This is exactly the right semantics for
/// coverage: a path was REACHED iff its marker opcode executed, independent of
/// whether the surrounding call later reverted.
#[derive(Default)]
struct LogCapture {
    logs: Vec<Log>,
}

impl<DB: Database> Inspector<DB> for LogCapture {
    fn log(&mut self, _interp: &mut Interpreter, _context: &mut EvmContext<DB>, log: &Log) {
        self.logs.push(log.clone());
    }
}

struct Args {
    bytecode_file: Option<String>,
    bytecode_inline: Option<String>,
    gas_limit: u64,
    /// Hex calldata (with or without 0x). When set, the deployed runtime is
    /// invoked WITH this calldata so a real selector-bearing contract dispatch
    /// runs the matching handler (path-coverage numerator = production dispatch,
    /// not the empty-calldata test-IO closure). Empty/None => empty calldata
    /// (legacy behaviour, byte-identical).
    calldata: Option<String>,
    /// File of hex calldata, ONE PER LINE, executed SEQUENTIALLY against a SINGLE
    /// persistent EVM/DB. State written by an earlier call (e.g. addMember) is
    /// visible to a later call (e.g. isMember/vote/tally), so handlers progress
    /// past their empty-state early reverts and reach their branch decision
    /// points. All LOG markers across the whole sequence are captured. This is
    /// how deep branch paths (loop bodies, rep checks, tally scoring) get hit.
    calls_file: Option<String>,
}

fn parse_args() -> Args {
    let mut a = Args {
        bytecode_file: None,
        bytecode_inline: None,
        gas_limit: 100_000_000,
        calldata: None,
        calls_file: None,
    };
    let mut it = env::args().skip(1);
    while let Some(arg) = it.next() {
        match arg.as_str() {
            // accepted for idris2-evm-run compatibility; trace output is always on
            "--trace" | "-t" => {}
            "--gas" => {
                if let Some(g) = it.next() {
                    a.gas_limit = g.parse().unwrap_or(100_000_000);
                }
            }
            "--bytecode" => {
                a.bytecode_inline = it.next();
            }
            "--calldata" => {
                a.calldata = it.next();
            }
            "--calls-file" => {
                a.calls_file = it.next();
            }
            // a bare token is the bytecode file; ignore other valueless flags
            other => {
                if !other.starts_with('-') && a.bytecode_file.is_none() {
                    a.bytecode_file = Some(other.to_string());
                }
            }
        }
    }
    a
}

fn load_calldata(args: &Args) -> Result<Vec<u8>, String> {
    match &args.calldata {
        None => Ok(Vec::new()),
        Some(s) => {
            let trimmed = s.trim();
            if trimmed.is_empty() {
                return Ok(Vec::new());
            }
            let stripped = trimmed
                .strip_prefix("0x")
                .or_else(|| trimmed.strip_prefix("0X"))
                .unwrap_or(trimmed);
            hex::decode(stripped).map_err(|e| format!("Invalid calldata hex: {e}"))
        }
    }
}

/// Parse the sequence of calldata from --calls-file (one hex line each).
/// Blank lines and `#` comments are skipped. Returns Ok(None) when no file.
/// Each call is (optional per-call caller, calldata). A line may carry an
/// optional `caller:0x<40hex>|` prefix to override the default CALLER for that
/// one call — this lets a sequence include a genuine NON-MEMBER call (needed to
/// reach a require*-style guard's failing arm) without faking it. Absent prefix
/// → default CALLER.
type Call = (Option<[u8; 20]>, Vec<u8>);

fn load_calls(args: &Args) -> Result<Option<Vec<Call>>, String> {
    let path = match &args.calls_file {
        None => return Ok(None),
        Some(p) => p,
    };
    let content =
        fs::read_to_string(path).map_err(|e| format!("Failed to read calls file: {e}"))?;
    let mut calls = Vec::new();
    for line in content.lines() {
        let t = line.trim();
        if t.is_empty() || t.starts_with('#') {
            continue;
        }
        let (caller_opt, data_str) = if let Some(rest) = t.strip_prefix("caller:") {
            match rest.split_once('|') {
                Some((addr_hex, data)) => {
                    let ah = addr_hex.strip_prefix("0x").or_else(|| addr_hex.strip_prefix("0X")).unwrap_or(addr_hex);
                    let ab = hex::decode(ah).map_err(|e| format!("Invalid caller hex '{addr_hex}': {e}"))?;
                    if ab.len() != 20 {
                        return Err(format!("caller must be 20 bytes, got {}", ab.len()));
                    }
                    let mut a = [0u8; 20];
                    a.copy_from_slice(&ab);
                    (Some(a), data)
                }
                None => return Err(format!("caller: prefix needs '|' separator: '{t}'")),
            }
        } else {
            (None, t)
        };
        let stripped = data_str.strip_prefix("0x").or_else(|| data_str.strip_prefix("0X")).unwrap_or(data_str);
        let bytes = hex::decode(stripped).map_err(|e| format!("Invalid calls hex '{data_str}': {e}"))?;
        calls.push((caller_opt, bytes));
    }
    Ok(Some(calls))
}

fn load_bytecode(args: &Args) -> Result<Vec<u8>, String> {
    let raw = if let Some(inline) = &args.bytecode_inline {
        inline.clone()
    } else if let Some(path) = &args.bytecode_file {
        fs::read_to_string(path).map_err(|e| format!("Failed to read file: {e}"))?
    } else {
        return Err("No bytecode specified".to_string());
    };
    let trimmed = raw.trim();
    let stripped = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
        .unwrap_or(trimmed);
    hex::decode(stripped).map_err(|e| format!("Invalid bytecode hex: {e}"))
}

fn main() {
    let args = parse_args();
    let code = match load_bytecode(&args) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {e}");
            process::exit(1);
        }
    };

    let calldata = match load_calldata(&args) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {e}");
            process::exit(1);
        }
    };

    let calls = match load_calls(&args) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {e}");
            process::exit(1);
        }
    };

    let callee = Address::from(CALLEE);
    let caller = Address::from(CALLER);

    // In-memory DB: install the runtime bytecode as the callee's account code,
    // and fund the caller so intrinsic gas is covered.
    let mut db = CacheDB::new(EmptyDB::default());
    let bytecode = Bytecode::new_raw(Bytes::from(code));
    db.insert_account_info(
        callee,
        AccountInfo {
            balance: U256::ZERO,
            nonce: 0,
            code_hash: bytecode.hash_slow(),
            code: Some(bytecode),
        },
    );
    db.insert_account_info(
        caller,
        AccountInfo {
            balance: U256::from(u128::MAX),
            nonce: 0,
            code_hash: revm::primitives::KECCAK_EMPTY,
            code: None,
        },
    );

    let mut evm = Evm::builder()
        .with_db(db)
        .with_external_context(LogCapture::default())
        .append_handler_register(inspector_handle_register)
        .modify_tx_env(|tx| {
            tx.caller = caller;
            tx.transact_to = TxKind::Call(callee);
            tx.data = Bytes::from(calldata.clone());
            tx.value = U256::ZERO;
            tx.gas_limit = args.gas_limit;
            tx.gas_price = U256::ZERO;
        })
        .modify_block_env(|b| {
            // basefee 0 + gas_price 0 means no base-fee check trips, so the
            // optional disable_base_fee cfg flag (feature-gated) is unnecessary.
            b.basefee = U256::ZERO;
        })
        .build();

    match calls {
        // SEQUENCE mode: run each calldata against a SINGLE persistent DB so
        // state from earlier calls (e.g. addMember) is visible to later ones
        // (e.g. isMember/vote/tally). transact_commit persists each call's state
        // changes. The inspector accumulates LOG markers across the whole
        // sequence; we print the aggregate at the end.
        Some(seq) => {
            let mut last: Option<ExecutionResult> = None;
            // Optional per-call block.timestamp advance (REVM_TIME_STEP seconds).
            // Some flows need time to pass BETWEEN calls within one sequence: e.g. a
            // proposal must be voted on while NOT expired, then tallied AFTER it expires
            // (finalTally only approves an expired proposal). With a constant timestamp
            // that is impossible. Advancing the block timestamp per call lets the
            // sequence simulate elapsed time, making the vote→(expire)→tally→execute
            // approval path reachable. Default 0 (unchanged behaviour) when unset.
            let time_step: u64 = std::env::var("REVM_TIME_STEP")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(0);
            // Per-caller nonce: each distinct caller address has its own nonce
            // sequence, so an injected non-member call (different caller) doesn't
            // desync the default caller's nonce.
            use std::collections::HashMap;
            let mut nonces: HashMap<[u8; 20], u64> = HashMap::new();
            let default_caller: [u8; 20] = caller.into();
            // Cursor into the accumulated marker log, advanced per call so we can
            // print only the markers that fired during the current call.
            let mut percall_marker_cursor: usize = 0;
            for (ci, (caller_opt, data)) in seq.iter().enumerate() {
                evm.tx_mut().data = Bytes::from(data.clone());
                let this_caller = caller_opt.unwrap_or(default_caller);
                evm.tx_mut().caller = Address::from(this_caller);
                // Advance the per-caller nonce to match committed account state,
                // otherwise revm rejects calls after the first with a nonce
                // mismatch (so addMember's sstore never commits and member-gated
                // True branches stay unreachable).
                let n = nonces.entry(this_caller).or_insert(0);
                evm.tx_mut().nonce = Some(*n);
                *n += 1;
                let _ = ci;
                if time_step > 0 {
                    evm.block_mut().timestamp = evm.block_mut().timestamp
                        .saturating_add(U256::from(time_step));
                }
                if std::env::var("REVM_DEBUG").is_ok() {
                    eprintln!("[call {ci}] ts={}", evm.block_mut().timestamp);
                }
                match evm.transact_commit() {
                    Ok(r) => {
                        if std::env::var("REVM_DEBUG").is_ok() {
                            match &r {
                                ExecutionResult::Success { output, .. } => {
                                    let ob = match output { Output::Call(b) => b.as_ref(), Output::Create(b, _) => b.as_ref() };
                                    eprintln!("[call {ci}] SUCCESS out=0x{}", hex::encode(ob));
                                }
                                ExecutionResult::Revert { output, .. } => eprintln!("[call {ci}] REVERT out=0x{}", hex::encode(output.as_ref())),
                                ExecutionResult::Halt { reason, .. } => eprintln!("[call {ci}] HALT {reason:?}"),
                            }
                        }
                        last = Some(r);
                    }
                    Err(_e) => {
                        if std::env::var("REVM_DEBUG").is_ok() {
                            eprintln!("[call {ci}] ENV-ERR {_e:?}");
                        }
                    }
                }
                // Per-call path-marker topics. The inspector captures every
                // log1(0,0,<path-id>) marker AS IT EXECUTES (before any revert
                // rolls it back), accumulating into context.external.logs. Print
                // the marker topics that fired DURING this call (the slice past
                // the previous call's end) so a reverting handler still reveals
                // exactly how far its control flow got — e.g. whether
                // requireRepProof's Ok-marker fired (rep passed, fail is later)
                // or only its Fail-marker fired. This is the observable signal
                // that replaces guessing which guard a revert came from.
                if std::env::var("REVM_PERCALL_MARKERS").is_ok() {
                    let total = evm.context.external.logs.len();
                    let start = percall_marker_cursor;
                    eprintln!("[call {ci}] markers this call: {}", total - start);
                    for lg in &evm.context.external.logs[start..total] {
                        for t in lg.topics() {
                            eprintln!("    marker 0x{}", hex::encode(t.as_slice()));
                        }
                    }
                    percall_marker_cursor = total;
                }
                if std::env::var("REVM_DUMP_STORAGE").is_ok() {
                    let callee_addr = Address::from(CALLEE);
                    if let Some(acc) = evm.context.evm.db.accounts.get(&callee_addr) {
                        let mut n = 0;
                        let mut slots: Vec<(U256, U256)> = acc.storage.iter().map(|(k, v)| (*k, *v)).collect();
                        slots.sort_by_key(|(k, _)| *k);
                        for (k, v) in &slots {
                            if *v != U256::ZERO {
                                n += 1;
                                eprintln!("    slot 0x{k:x} = 0x{v:x}");
                            }
                        }
                        eprintln!("[call {ci}] non-zero storage slots: {n}");
                    }
                }
            }
            let captured = evm.context.external.logs.clone();
            print_trace_with_logs(last.as_ref(), &captured);
        }
        // SINGLE mode (legacy + --calldata): one transaction, do not commit.
        None => {
            let result = match evm.transact() {
                Ok(r) => r.result,
                Err(e) => {
                    let captured = evm.context.external.logs.clone();
                    print_trace_with_logs(None, &captured);
                    println!("Result: ERROR ({e})");
                    return;
                }
            };
            // Prefer the inspector-captured logs (these survive reverts) — the
            // honest "path reached" signal. Superset of result.logs(), identical
            // on success.
            let captured = evm.context.external.logs.clone();
            print_trace_with_logs(Some(&result), &captured);
        }
    }
}

fn print_trace_with_logs(result: Option<&ExecutionResult>, logs: &[Log]) {
    if !logs.is_empty() {
        println!("\n=== Logs ({} events) ===", logs.len());
        for (i, log) in logs.iter().enumerate() {
            println!("Log #{i}:");
            let topics = log.topics();
            println!("  Topics: {}", topics.len());
            for (j, topic) in topics.iter().enumerate() {
                println!("    [{j}] 0x{}", hex::encode(topic.as_slice()));
            }
            let data = log.data.data.as_ref();
            println!("  Data: {} bytes", data.len());
            if !data.is_empty() {
                println!("    0x{}", hex::encode(data));
            }
        }
    }

    match result {
        None => {}
        Some(ExecutionResult::Success { gas_used, output, .. }) => {
            println!("Result: SUCCESS (gas used: {gas_used})");
            if let Output::Call(bytes) = output {
                // coverage parser ignores return data; omitting keeps parity
                let _ = bytes;
            }
        }
        Some(ExecutionResult::Revert { gas_used, .. }) => {
            // On a real EVM revert, committed logs roll back. For path coverage we
            // print the inspector-captured logs above (markers that fired BEFORE
            // the revert), which is the correct "path reached" signal. The result
            // line still reports REVERT for honest execution status.
            println!("Result: REVERT (gas used: {gas_used})");
        }
        Some(ExecutionResult::Halt { reason, gas_used }) => {
            println!("Result: HALT {reason:?} (gas used: {gas_used})");
        }
    }
}
