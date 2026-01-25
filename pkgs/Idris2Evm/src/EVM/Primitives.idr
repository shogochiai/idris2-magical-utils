||| EVM Primitives - Consolidated FFI Definitions
|||
||| This is the ONLY place where EVM FFI primitives are defined.
||| All other modules should import from here.
|||
||| Reference: https://www.evm.codes/
module EVM.Primitives

-- =============================================================================
-- Storage Operations (0x54-0x55)
-- =============================================================================

%foreign "evm:sload"
export prim__sload : Integer -> PrimIO Integer

%foreign "evm:sstore"
export prim__sstore : Integer -> Integer -> PrimIO ()

export
sload : Integer -> IO Integer
sload slot = primIO (prim__sload slot)

export
sstore : Integer -> Integer -> IO ()
sstore slot val = primIO (prim__sstore slot val)

-- =============================================================================
-- Memory Operations (0x51-0x53)
-- =============================================================================

%foreign "evm:mload"
export prim__mload : Integer -> PrimIO Integer

%foreign "evm:mstore"
export prim__mstore : Integer -> Integer -> PrimIO ()

%foreign "evm:mstore8"
export prim__mstore8 : Integer -> Integer -> PrimIO ()

export
mload : Integer -> IO Integer
mload off = primIO (prim__mload off)

export
mstore : Integer -> Integer -> IO ()
mstore off val = primIO (prim__mstore off val)

export
mstore8 : Integer -> Integer -> IO ()
mstore8 off val = primIO (prim__mstore8 off val)

-- =============================================================================
-- Calldata Operations (0x35-0x37)
-- =============================================================================

%foreign "evm:calldataload"
export prim__calldataload : Integer -> PrimIO Integer

%foreign "evm:calldatasize"
export prim__calldatasize : PrimIO Integer

%foreign "evm:calldatacopy"
export prim__calldatacopy : Integer -> Integer -> Integer -> PrimIO ()

export
calldataload : Integer -> IO Integer
calldataload off = primIO (prim__calldataload off)

export
calldatasize : IO Integer
calldatasize = primIO prim__calldatasize

export
calldatacopy : Integer -> Integer -> Integer -> IO ()
calldatacopy destOff srcOff len = primIO (prim__calldatacopy destOff srcOff len)

-- =============================================================================
-- Return Data Operations (0x3d-0x3e)
-- =============================================================================

%foreign "evm:returndatasize"
export prim__returndatasize : PrimIO Integer

%foreign "evm:returndatacopy"
export prim__returndatacopy : Integer -> Integer -> Integer -> PrimIO ()

export
returndatasize : IO Integer
returndatasize = primIO prim__returndatasize

export
returndatacopy : Integer -> Integer -> Integer -> IO ()
returndatacopy destOff srcOff len = primIO (prim__returndatacopy destOff srcOff len)

-- =============================================================================
-- Environment Operations (0x30-0x3f)
-- =============================================================================

%foreign "evm:caller"
export prim__caller : PrimIO Integer

%foreign "evm:callvalue"
export prim__callvalue : PrimIO Integer

%foreign "evm:address"
export prim__address : PrimIO Integer

%foreign "evm:origin"
export prim__origin : PrimIO Integer

%foreign "evm:gas"
export prim__gas : PrimIO Integer

%foreign "evm:gasprice"
export prim__gasprice : PrimIO Integer

%foreign "evm:balance"
export prim__balance : Integer -> PrimIO Integer

%foreign "evm:selfbalance"
export prim__selfbalance : PrimIO Integer

%foreign "evm:codesize"
export prim__codesize : PrimIO Integer

export
caller : IO Integer
caller = primIO prim__caller

export
callvalue : IO Integer
callvalue = primIO prim__callvalue

export
address : IO Integer
address = primIO prim__address

export
origin : IO Integer
origin = primIO prim__origin

export
gas : IO Integer
gas = primIO prim__gas

export
gasprice : IO Integer
gasprice = primIO prim__gasprice

export
balance : Integer -> IO Integer
balance addr = primIO (prim__balance addr)

export
selfbalance : IO Integer
selfbalance = primIO prim__selfbalance

export
codesize : IO Integer
codesize = primIO prim__codesize

-- =============================================================================
-- Block Operations (0x40-0x48)
-- =============================================================================

%foreign "evm:blockhash"
export prim__blockhash : Integer -> PrimIO Integer

%foreign "evm:coinbase"
export prim__coinbase : PrimIO Integer

%foreign "evm:timestamp"
export prim__timestamp : PrimIO Integer

%foreign "evm:number"
export prim__number : PrimIO Integer

%foreign "evm:difficulty"
export prim__difficulty : PrimIO Integer

%foreign "evm:gaslimit"
export prim__gaslimit : PrimIO Integer

%foreign "evm:chainid"
export prim__chainid : PrimIO Integer

%foreign "evm:basefee"
export prim__basefee : PrimIO Integer

export
blockhash : Integer -> IO Integer
blockhash blockNum = primIO (prim__blockhash blockNum)

export
coinbase : IO Integer
coinbase = primIO prim__coinbase

export
timestamp : IO Integer
timestamp = primIO prim__timestamp

export
number : IO Integer
number = primIO prim__number

export
difficulty : IO Integer
difficulty = primIO prim__difficulty

export
gaslimit : IO Integer
gaslimit = primIO prim__gaslimit

export
chainid : IO Integer
chainid = primIO prim__chainid

export
basefee : IO Integer
basefee = primIO prim__basefee

-- =============================================================================
-- Control Flow (0x00, 0xf3, 0xfd)
-- =============================================================================

%foreign "evm:stop"
export prim__stop : PrimIO ()

%foreign "evm:return"
export prim__return : Integer -> Integer -> PrimIO ()

%foreign "evm:revert"
export prim__revert : Integer -> Integer -> PrimIO ()

export
stop : IO ()
stop = primIO prim__stop

export
evmReturn : Integer -> Integer -> IO ()
evmReturn off len = primIO (prim__return off len)

export
evmRevert : Integer -> Integer -> IO ()
evmRevert off len = primIO (prim__revert off len)

-- =============================================================================
-- Call Operations (0xf1, 0xf2, 0xf4, 0xfa)
-- =============================================================================

%foreign "evm:call"
export prim__call : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> PrimIO Integer

%foreign "evm:callcode"
export prim__callcode : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> PrimIO Integer

%foreign "evm:delegatecall"
export prim__delegatecall : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> PrimIO Integer

%foreign "evm:staticcall"
export prim__staticcall : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> PrimIO Integer

export
call : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO Integer
call g addr value argsOff argsSize retOff retSize =
  primIO (prim__call g addr value argsOff argsSize retOff retSize)

export
callcode : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO Integer
callcode g addr value argsOff argsSize retOff retSize =
  primIO (prim__callcode g addr value argsOff argsSize retOff retSize)

export
delegatecall : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO Integer
delegatecall g addr argsOff argsSize retOff retSize =
  primIO (prim__delegatecall g addr argsOff argsSize retOff retSize)

export
staticcall : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO Integer
staticcall g addr argsOff argsSize retOff retSize =
  primIO (prim__staticcall g addr argsOff argsSize retOff retSize)

-- =============================================================================
-- Create Operations (0xf0, 0xf5)
-- =============================================================================

%foreign "evm:create"
export prim__create : Integer -> Integer -> Integer -> PrimIO Integer

%foreign "evm:create2"
export prim__create2 : Integer -> Integer -> Integer -> Integer -> PrimIO Integer

export
create : Integer -> Integer -> Integer -> IO Integer
create value off size = primIO (prim__create value off size)

export
create2 : Integer -> Integer -> Integer -> Integer -> IO Integer
create2 value off size salt = primIO (prim__create2 value off size salt)

-- =============================================================================
-- Log Operations (0xa0-0xa4)
-- =============================================================================

%foreign "evm:log0"
export prim__log0 : Integer -> Integer -> PrimIO ()

%foreign "evm:log1"
export prim__log1 : Integer -> Integer -> Integer -> PrimIO ()

%foreign "evm:log2"
export prim__log2 : Integer -> Integer -> Integer -> Integer -> PrimIO ()

%foreign "evm:log3"
export prim__log3 : Integer -> Integer -> Integer -> Integer -> Integer -> PrimIO ()

%foreign "evm:log4"
export prim__log4 : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> PrimIO ()

export
log0 : Integer -> Integer -> IO ()
log0 off size = primIO (prim__log0 off size)

export
log1 : Integer -> Integer -> Integer -> IO ()
log1 off size topic1 = primIO (prim__log1 off size topic1)

export
log2 : Integer -> Integer -> Integer -> Integer -> IO ()
log2 off size topic1 topic2 = primIO (prim__log2 off size topic1 topic2)

export
log3 : Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
log3 off size topic1 topic2 topic3 = primIO (prim__log3 off size topic1 topic2 topic3)

export
log4 : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
log4 off size topic1 topic2 topic3 topic4 = primIO (prim__log4 off size topic1 topic2 topic3 topic4)

-- =============================================================================
-- Crypto Operations (0x20)
-- =============================================================================

%foreign "evm:keccak256"
export prim__keccak256 : Integer -> Integer -> PrimIO Integer

export
keccak256 : Integer -> Integer -> IO Integer
keccak256 off len = primIO (prim__keccak256 off len)

-- =============================================================================
-- Bitwise Operations (for special cases)
-- =============================================================================

%foreign "evm:shl"
export prim__shl : Integer -> Integer -> PrimIO Integer

%foreign "evm:shr"
export prim__shr : Integer -> Integer -> PrimIO Integer

%foreign "evm:sar"
export prim__sar : Integer -> Integer -> PrimIO Integer

%foreign "evm:and"
export prim__and : Integer -> Integer -> PrimIO Integer

%foreign "evm:or"
export prim__or : Integer -> Integer -> PrimIO Integer

%foreign "evm:xor"
export prim__xor : Integer -> Integer -> PrimIO Integer

%foreign "evm:not"
export prim__not : Integer -> PrimIO Integer

export
shl : Integer -> Integer -> IO Integer
shl shift val = primIO (prim__shl shift val)

export
shr : Integer -> Integer -> IO Integer
shr shift val = primIO (prim__shr shift val)

export
sar : Integer -> Integer -> IO Integer
sar shift val = primIO (prim__sar shift val)

export
and : Integer -> Integer -> IO Integer
and a b = primIO (prim__and a b)

export
or : Integer -> Integer -> IO Integer
or a b = primIO (prim__or a b)

export
xor : Integer -> Integer -> IO Integer
xor a b = primIO (prim__xor a b)

export
not : Integer -> IO Integer
not a = primIO (prim__not a)

-- =============================================================================
-- Self Destruct (0xff)
-- =============================================================================

%foreign "evm:selfdestruct"
export prim__selfdestruct : Integer -> PrimIO ()

export
selfdestruct : Integer -> IO ()
selfdestruct recipient = primIO (prim__selfdestruct recipient)

-- =============================================================================
-- Compound Operations (idris2-yul specific)
-- =============================================================================

||| Return or revert based on success flag
||| Used for proxy forwarding pattern
%foreign "evm:returnOrRevert"
export prim__returnOrRevert : Integer -> Integer -> Integer -> PrimIO ()

export
returnOrRevert : Integer -> Integer -> Integer -> IO ()
returnOrRevert success off len = primIO (prim__returnOrRevert success off len)

-- =============================================================================
-- Convenience Helpers
-- =============================================================================

||| Extract function selector from calldata (first 4 bytes)
export
getSelector : IO Integer
getSelector = do
  data_ <- calldataload 0
  -- Shift right 224 bits (256 - 32) to get first 4 bytes
  pure (data_ `div` 0x100000000000000000000000000000000000000000000000000000000)

||| Return a uint256 value
export
returnUint : Integer -> IO ()
returnUint val = do
  mstore 0 val
  evmReturn 0 32

||| Return a boolean value
export
returnBool : Bool -> IO ()
returnBool b = returnUint (if b then 1 else 0)
