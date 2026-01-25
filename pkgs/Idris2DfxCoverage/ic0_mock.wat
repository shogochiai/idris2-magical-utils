;; IC0 Mock Module for wasmtime
;; Provides stub implementations of IC System API functions

(module
  ;; Memory for data exchange
  (memory (export "memory") 1)

  ;; Global state
  (global $msg_arg_size (mut i32) (i32.const 6))  ;; DIDL\x00\x00
  (global $caller_size (mut i32) (i32.const 29))
  (global $canister_id_size (mut i32) (i32.const 10))
  (global $cycle_balance (mut i64) (i64.const 1000000000000))
  (global $current_time (mut i64) (i64.const 1700000000000000000))

  ;; msg_arg_data_size : () -> i32
  (func (export "msg_arg_data_size") (result i32)
    global.get $msg_arg_size
  )

  ;; msg_arg_data_copy : (dst: i32, offset: i32, size: i32) -> ()
  (func (export "msg_arg_data_copy") (param $dst i32) (param $offset i32) (param $size i32)
    ;; Just write DIDL\x00\x00 (empty Candid)
    (i32.store8 (local.get $dst) (i32.const 0x44))  ;; D
    (i32.store8 (i32.add (local.get $dst) (i32.const 1)) (i32.const 0x49))  ;; I
    (i32.store8 (i32.add (local.get $dst) (i32.const 2)) (i32.const 0x44))  ;; D
    (i32.store8 (i32.add (local.get $dst) (i32.const 3)) (i32.const 0x4C))  ;; L
    (i32.store8 (i32.add (local.get $dst) (i32.const 4)) (i32.const 0x00))
    (i32.store8 (i32.add (local.get $dst) (i32.const 5)) (i32.const 0x00))
  )

  ;; msg_caller_size : () -> i32
  (func (export "msg_caller_size") (result i32)
    global.get $caller_size
  )

  ;; msg_caller_copy : (dst: i32, offset: i32, size: i32) -> ()
  (func (export "msg_caller_copy") (param $dst i32) (param $offset i32) (param $size i32)
    ;; Write zeros (anonymous caller)
  )

  ;; msg_reply_data_append : (src: i32, size: i32) -> ()
  (func (export "msg_reply_data_append") (param $src i32) (param $size i32)
    ;; No-op, just record that data was appended
  )

  ;; msg_reply : () -> ()
  (func (export "msg_reply")
    ;; No-op
  )

  ;; msg_reject : (src: i32, size: i32) -> ()
  (func (export "msg_reject") (param $src i32) (param $size i32)
    ;; No-op
  )

  ;; canister_self_size : () -> i32
  (func (export "canister_self_size") (result i32)
    global.get $canister_id_size
  )

  ;; canister_self_copy : (dst: i32, offset: i32, size: i32) -> ()
  (func (export "canister_self_copy") (param $dst i32) (param $offset i32) (param $size i32)
    ;; Write default canister ID
  )

  ;; canister_cycle_balance : () -> i64
  (func (export "canister_cycle_balance") (result i64)
    global.get $cycle_balance
  )

  ;; canister_cycle_balance128 : (dst: i32) -> ()
  (func (export "canister_cycle_balance128") (param $dst i32)
    ;; Write 128-bit balance (just lower 64 bits)
    (i64.store (local.get $dst) (global.get $cycle_balance))
    (i64.store (i32.add (local.get $dst) (i32.const 8)) (i64.const 0))
  )

  ;; time : () -> i64
  (func (export "time") (result i64)
    global.get $current_time
  )

  ;; debug_print : (src: i32, size: i32) -> ()
  (func (export "debug_print") (param $src i32) (param $size i32)
    ;; No-op (would print to console in real impl)
  )

  ;; trap : (src: i32, size: i32) -> ()
  (func (export "trap") (param $src i32) (param $size i32)
    unreachable
  )

  ;; stable_size : () -> i32
  (func (export "stable_size") (result i32)
    i32.const 0
  )

  ;; stable_grow : (pages: i32) -> i32
  (func (export "stable_grow") (param $pages i32) (result i32)
    i32.const 0  ;; Success
  )

  ;; stable_read : (dst: i32, offset: i32, size: i32) -> ()
  (func (export "stable_read") (param $dst i32) (param $offset i32) (param $size i32)
    ;; No-op
  )

  ;; stable_write : (offset: i32, src: i32, size: i32) -> ()
  (func (export "stable_write") (param $offset i32) (param $src i32) (param $size i32)
    ;; No-op
  )

  ;; stable64_size : () -> i64
  (func (export "stable64_size") (result i64)
    i64.const 0
  )

  ;; stable64_grow : (pages: i64) -> i64
  (func (export "stable64_grow") (param $pages i64) (result i64)
    i64.const 0
  )

  ;; stable64_read : (dst: i64, offset: i64, size: i64) -> ()
  (func (export "stable64_read") (param $dst i64) (param $offset i64) (param $size i64)
    ;; No-op
  )

  ;; stable64_write : (offset: i64, src: i64, size: i64) -> ()
  (func (export "stable64_write") (param $offset i64) (param $src i64) (param $size i64)
    ;; No-op
  )

  ;; performance_counter : (type: i32) -> i64
  (func (export "performance_counter") (param $type i32) (result i64)
    i64.const 0
  )

  ;; call_new and related - minimal stubs
  (func (export "call_new")
    (param i32) (param i32) (param i32) (param i32)
    (param i32) (param i32) (param i32) (param i32)
    ;; No-op
  )

  (func (export "call_on_cleanup") (param i32) (param i32)
    ;; No-op
  )

  (func (export "call_data_append") (param i32) (param i32)
    ;; No-op
  )

  (func (export "call_cycles_add") (param i64)
    ;; No-op
  )

  (func (export "call_cycles_add128") (param i64) (param i64)
    ;; No-op
  )

  (func (export "call_perform") (result i32)
    i32.const 0  ;; Success
  )
)
