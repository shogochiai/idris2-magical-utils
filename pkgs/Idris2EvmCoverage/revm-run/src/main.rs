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
}

fn parse_args() -> Args {
    let mut a = Args {
        bytecode_file: None,
        bytecode_inline: None,
        gas_limit: 100_000_000,
        calldata: None,
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

    let result = match evm.transact() {
        Ok(r) => r.result,
        Err(e) => {
            // Execution-environment error (not a normal revert). Print whatever
            // markers fired before the error so coverage is not silently zeroed.
            let captured = evm.context.external.logs.clone();
            print_trace_with_logs(None, &captured);
            println!("Result: ERROR ({e})");
            return;
        }
    };

    // Prefer the inspector-captured logs (these survive reverts), which is the
    // honest "path reached" signal for coverage. They are a superset of
    // result.logs() (committed logs) and identical on success.
    let captured = evm.context.external.logs.clone();
    print_trace_with_logs(Some(&result), &captured);
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
