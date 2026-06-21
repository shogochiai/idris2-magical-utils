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
    primitives::{
        AccountInfo, Address, Bytecode, Bytes, ExecutionResult, Output, TxKind, U256,
    },
    Evm,
};

const CALLEE: [u8; 20] = [0x10; 20];
const CALLER: [u8; 20] = [0x20; 20];

struct Args {
    bytecode_file: Option<String>,
    bytecode_inline: Option<String>,
    gas_limit: u64,
}

fn parse_args() -> Args {
    let mut a = Args {
        bytecode_file: None,
        bytecode_inline: None,
        gas_limit: 100_000_000,
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
        .modify_tx_env(|tx| {
            tx.caller = caller;
            tx.transact_to = TxKind::Call(callee);
            tx.data = Bytes::new();
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
            // Execution-environment error (not a normal revert). Print no logs and
            // a non-SUCCESS result line so the parser yields zero counters rather
            // than crashing.
            println!("Result: ERROR ({e})");
            return;
        }
    };

    print_trace(&result);
}

fn print_trace(result: &ExecutionResult) {
    let logs = result.logs();

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
        ExecutionResult::Success { gas_used, output, .. } => {
            println!("Result: SUCCESS (gas used: {gas_used})");
            if let Output::Call(bytes) = output {
                // coverage parser ignores return data; omitting keeps parity
                let _ = bytes;
            }
        }
        ExecutionResult::Revert { gas_used, .. } => {
            // On a real EVM revert, logs roll back. The instrumentor inserts a
            // ProfileFlush BEFORE revert/return so pre-revert counters still emit;
            // revm's logs() reflects the same EIP semantics as idris2-evm-run.
            println!("Result: REVERT (gas used: {gas_used})");
        }
        ExecutionResult::Halt { reason, gas_used } => {
            println!("Result: HALT {reason:?} (gas used: {gas_used})");
        }
    }
}
