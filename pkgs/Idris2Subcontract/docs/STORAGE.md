# EVM Storage Layout Guide

This guide explains how Solidity storage layout works and how idris2-subcontract implements it.

## Background: EVM Storage Model

EVM storage is a key-value store where:
- Keys are 256-bit slot numbers (0, 1, 2, ...)
- Values are 256-bit words (32 bytes)

Solidity assigns storage slots to variables based on declaration order and type.

## ERC-7201: Namespaced Storage

[EIP-7201](https://eips.ethereum.org/EIPS/eip-7201) defines a standard for computing storage locations that:
- Avoids slot collisions between upgradeable contracts
- Provides deterministic, verifiable slot locations
- Aligns to 256-byte boundaries for future compatibility

### Formula

```
slot = keccak256(keccak256(namespace_id) - 1) & ~0xff
```

Where:
- `namespace_id` is a string like `"myapp.storage"`
- `& ~0xff` masks the lowest 8 bits to zero (256-byte alignment)

### Computing Slots

**Using cast (Foundry):**
```bash
# Step 1: Hash the namespace
HASH=$(cast keccak "myapp.storage")
# Example: 0x3a4d5e6f...

# Step 2: Subtract 1
MINUS_ONE=$(python3 -c "print(hex(int('$HASH', 16) - 1))")

# Step 3: Hash again
HASH2=$(cast keccak $(printf '%064s' ${MINUS_ONE#0x} | tr ' ' '0'))

# Step 4: Align
SLOT=$(python3 -c "print(hex(int('$HASH2', 16) & ~0xff))")
```

**Using Solidity:**
```solidity
function computeSlot(string memory id) pure returns (bytes32) {
    return keccak256(abi.encode(keccak256(bytes(id)) - 1)) & ~bytes32(uint256(0xff));
}
```

**Using idris2-yul at runtime:**
```idris
import EVM.Storage.Namespace

-- From pre-computed namespace hash
slot <- erc7201FromHash 0x3a4d5e6f...
```

## Solidity Storage Layout Rules

### Value Types

Simple types occupy one slot each:
```solidity
uint256 a;   // slot 0
uint256 b;   // slot 1
address c;   // slot 2
bool d;      // slot 3
```

In idris2-subcontract:
```idris
import EVM.Primitives
import EVM.Storage.Namespace

let slotA = structFieldSlot baseSlot 0
let slotB = structFieldSlot baseSlot 1
```

### Mappings

Mappings use keccak256 to compute element slots:
```
slot(mapping[key]) = keccak256(key . mappingSlot)
```

Where `.` is concatenation in memory (key at offset 0, slot at offset 32).

```solidity
mapping(address => uint256) balances;  // base slot 0
// balances[addr] is at keccak256(addr . 0)
```

In idris2-subcontract:
```idris
import EVM.Primitives
import EVM.Storage.Namespace

slot <- mappingSlot 0 addr
value <- sload slot
```

### Nested Mappings

For `mapping(K1 => mapping(K2 => V))`:
```
slot = keccak256(key2 . keccak256(key1 . baseSlot))
```

```solidity
mapping(address => mapping(address => uint256)) allowances;
// allowances[owner][spender] is at:
// keccak256(spender . keccak256(owner . baseSlot))
```

In idris2-subcontract:
```idris
slot <- nestedMappingSlot baseSlot owner spender
```

### Dynamic Arrays

Array length is stored at the base slot. Array data starts at `keccak256(baseSlot)`:
```
array.length at: baseSlot
array[i] at: keccak256(baseSlot) + i * elementSize
```

```solidity
address[] members;  // base slot 0
// members.length at slot 0
// members[0] at keccak256(0)
// members[1] at keccak256(0) + 1
```

In idris2-subcontract:
```idris
import EVM.Primitives
import EVM.Storage.Namespace

len <- arrayLength baseSlot
elementSlot <- arrayElementSlot baseSlot 3 1  -- members[3], 1 slot per element
```

### Structs

Struct fields are stored contiguously:
```solidity
struct User {
    address addr;    // offset 0
    uint256 balance; // offset 1
    bool active;     // offset 2
}
```

In idris2-subcontract:
```idris
let addrSlot = structFieldSlot userSlot 0
let balanceSlot = structFieldSlot userSlot 1
let activeSlot = structFieldSlot userSlot 2
```

### Structs in Mappings

For `mapping(K => Struct)`, each struct starts at the computed slot:
```
struct.field = keccak256(key . baseSlot) + fieldOffset
```

In idris2-subcontract:
```idris
userSlot <- mappingSlot usersBaseSlot addr
let balanceSlot = structFieldSlot userSlot 1
```

### Arrays of Structs

For `Struct[]`, each struct element:
```
array[i].field = keccak256(baseSlot) + i * structSize + fieldOffset
```

In idris2-subcontract:
```idris
-- Get slot for users[5].balance (struct size 3, balance at offset 1)
elemSlot <- arrayElementSlot baseSlot 5 3
let balanceSlot = structFieldSlot elemSlot 1
```

## Complete Example

```solidity
/// @custom:storage-location erc7201:myapp.token
struct $Token {
    uint256 totalSupply;                                    // offset 0
    mapping(address => uint256) balances;                   // offset 1
    mapping(address => mapping(address => uint256)) allow;  // offset 2
    address[] holders;                                      // offset 3
}
```

```idris
import EVM.Primitives
import EVM.Storage.Namespace

-- Pre-computed: keccak256(keccak256("myapp.token") - 1) & ~0xff
TOKEN_SLOT : Integer
TOKEN_SLOT = 0x...

-- Read total supply
getTotalSupply : IO Integer
getTotalSupply = sload TOKEN_SLOT

-- Read balance
getBalance : Integer -> IO Integer
getBalance addr = do
  let baseSlot = TOKEN_SLOT + 1
  slot <- mappingSlot baseSlot addr
  sload slot

-- Read allowance
getAllowance : Integer -> Integer -> IO Integer
getAllowance owner spender = do
  let baseSlot = TOKEN_SLOT + 2
  slot <- nestedMappingSlot baseSlot owner spender
  sload slot

-- Get holder count
getHolderCount : IO Integer
getHolderCount = arrayLength (TOKEN_SLOT + 3)

-- Get holder at index
getHolderAt : Integer -> IO Integer
getHolderAt idx = do
  slot <- arrayElementSlot (TOKEN_SLOT + 3) idx 1
  readAddress slot
```

## Using StorageCap for Controlled Access

For more controlled storage access, use the capability pattern:

```idris
import Subcontract.Core.StorageCap

-- Handler that reads balance (requires StorageCap)
getBalanceHandler : Integer -> Handler Integer
getBalanceHandler addr cap = do
  slot <- mappingSlotCap cap (TOKEN_SLOT + 1) addr
  sloadCap cap slot

-- Framework provides capability
main : IO ()
main = do
  addr <- caller
  balance <- runHandler (getBalanceHandler addr)
  returnUint balance
```

This pattern makes storage access explicit in function signatures.

## Packed Storage (Advanced)

Solidity packs smaller types into single slots:
```solidity
struct Packed {
    uint128 a;   // slot 0, bytes 0-15
    uint64 b;    // slot 0, bytes 16-23
    uint64 c;    // slot 0, bytes 24-31
}
```

idris2-subcontract currently treats all types as full slots. For packed storage, use bit manipulation:
```idris
-- Read uint128 from lower 128 bits
readLower128 : Integer -> IO Integer
readLower128 slot = do
  val <- sload slot
  pure (val `mod` (2 `pow` 128))

-- Read uint64 from bits 128-191
readMiddle64 : Integer -> IO Integer
readMiddle64 slot = do
  val <- sload slot
  pure ((val `div` (2 `pow` 128)) `mod` (2 `pow` 64))
```

## Best Practices

1. **Pre-compute slots offline** - Don't compute ERC-7201 slots on-chain
2. **Use meaningful namespace IDs** - e.g., `"myorg.myapp.v1.storage"`
3. **Document slot assignments** - Keep a mapping of fields to offsets
4. **Test with known values** - Verify slot calculations match Solidity
5. **Consider upgrades** - Use ERC-7201 for proxy-safe storage
6. **Use StorageCap** - Make storage access explicit in type signatures
7. **Validate upgrades at compile-time** - Use `checkUpgrade` macro (see below)

## Compile-Time Schema Upgrade Validation

idris2-subcontract provides **compile-time storage collision detection** using Idris2's elaborator reflection. This prevents the most dangerous class of EVM bugs—storage slot collisions during upgrades—before the code even compiles.

### The Problem

When upgrading a proxy contract, changing the storage layout can corrupt existing data:

```solidity
// V1: Deployed and has user data
struct TokenV1 {
    address owner;       // slot 0
    uint256 totalSupply; // slot 1
}

// V2: DANGEROUS - reordered fields!
struct TokenV2 {
    uint256 totalSupply; // slot 0 - now reads owner's address as a number!
    address owner;       // slot 1 - now reads totalSupply as an address!
}
```

### The Solution: Compile-Time Validation

```idris
import Subcontract.Core.Schema
import Subcontract.Core.SchemaCheck

-- V1: Currently deployed
TokenSchemaV1 : Schema
TokenSchemaV1 = MkSchema "myapp.token" 0x1234
  [ Value "owner" TAddress
  , Value "totalSupply" TUint256
  ]

-- V2: Proposed upgrade (SAFE - append-only)
TokenSchemaV2 : Schema
TokenSchemaV2 = MkSchema "myapp.token" 0x1234
  [ Value "owner" TAddress
  , Value "totalSupply" TUint256
  , Value "paused" TBool         -- NEW field appended at end
  ]

-- Compile-time validation: fails if unsafe
%runElab checkUpgrade TokenSchemaV1 TokenSchemaV2
```

If the upgrade is safe, compilation proceeds. If not, you get a detailed error:

```
SCHEMA UPGRADE VALIDATION FAILED
================================

=== STORAGE COLLISION: TYPE_CHANGED ===
Field: owner
Old position: slot+0
New position: slot+0
Old definition: owner : address
New definition: owner : uint256
Reason: Type changed from address to uint256

Schema upgrades must be append-only:
  - Existing fields cannot be removed
  - Existing fields cannot be reordered
  - Field types cannot be changed
  - New fields must be appended at the end
```

### Collision Types Detected

| Collision Type | Description |
|----------------|-------------|
| `FIELD_REMOVED` | A field from V1 is missing in V2 |
| `FIELD_REORDERED` | A field exists but at a different slot position |
| `TYPE_CHANGED` | Same field name, but different type |
| `NAMESPACE_CHANGED` | ERC-7201 namespace ID changed |
| `ROOT_SLOT_CHANGED` | Root storage slot changed |

### ERC-7546 Governance Integration

For contracts using OptimisticUpgrader or Inception governance:

```idris
import Subcontract.Standards.ERC7546.UpgradeValidation

-- Governance-aware validation with actionable error messages
%runElab validateUpgradeProposal TokenSchemaV1 TokenSchemaV2
```

Error messages include governance guidance:

```
ERC-7546 UPGRADE BLOCKED
========================

Resolution Options:
  1. Fix schema to be append-only compatible
  2. Deploy migration contract to relocate data
  3. Create new namespace for breaking changes

For OptimisticUpgrader: This proposal should be REJECTED
For Inception: This violates storage safety boundary
```

### API Reference

```idris
-- Core validation (Elab macro)
checkUpgrade : Schema -> Schema -> Elab ()

-- Alternative syntax
assertUpgradeSafe : Schema -> Schema -> Elab ()  -- %macro

-- ERC-7546 governance integration
validateUpgradeProposal : Schema -> Schema -> Elab ()

-- Pure validation (for runtime/testing)
checkSchemaCompat : Schema -> Schema -> CompatResult
isCompatible : CompatResult -> Bool
```

### When to Use

- **Before deploying upgrades** - Add `%runElab checkUpgrade` to your upgrade scripts
- **In CI/CD pipelines** - Schema mismatches fail compilation automatically
- **During code review** - Reviewers can trust that storage safety is enforced
- **With governance** - Use `validateUpgradeProposal` in ERC-7546 workflows
