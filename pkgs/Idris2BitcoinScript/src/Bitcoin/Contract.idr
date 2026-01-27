||| Bitcoin Smart Contract Abstraction
|||
||| Type-safe contract definition with method dispatch.
||| Inspired by sCrypt's contract structure.
module Bitcoin.Contract

import Bitcoin.Opcode
import Bitcoin.Script
import Bitcoin.Patterns
import Data.Vect
import Data.List

%default total

-- =============================================================================
-- Method Definition
-- =============================================================================

||| A contract method with selector and script
||| The script transforms witness stack to result
public export
record Method where
  constructor MkMethod
  name : String
  selector : Int          -- 1-byte method selector (0x01-0xFF)
  -- Script is existentially quantified over stack types
  -- For simplicity, we store compiled bytes
  compiled : List Int

||| Create a method from a script
||| Requires explicit stack types for compilation
public export
method : String -> Int -> Script before after -> Method
method name sel script = MkMethod name sel (compile script)

-- =============================================================================
-- Contract Definition
-- =============================================================================

||| A smart contract with multiple methods
public export
record Contract where
  constructor MkContract
  name : String
  version : Int
  methods : List Method

||| Create an empty contract
public export
emptyContract : String -> Contract
emptyContract name = MkContract name 1 []

||| Add a method to a contract
public export
addMethod : Method -> Contract -> Contract
addMethod m c = { methods $= (m ::) } c

-- =============================================================================
-- Contract Compilation
-- =============================================================================

||| Compile a single method branch
||| OP_DUP <selector> OP_EQUAL OP_IF <script> OP_ENDIF
compileMethodBranch : Method -> List Int
compileMethodBranch m =
  [ opcodeByte OP_DUP
  , 0x01, m.selector           -- Push 1-byte selector
  , opcodeByte OP_EQUAL
  , opcodeByte OP_IF
  ] ++ m.compiled ++
  [ opcodeByte OP_ENDIF ]

||| Compile contract to Bitcoin Script bytes
||| Structure: check each method selector in sequence
|||
||| Witness: <selector> <method-specific-data...>
public export
compileContract : Contract -> List Int
compileContract c =
  -- Drop selector after all checks, assuming one matched
  let methodBranches = concatMap compileMethodBranch c.methods
  in methodBranches ++ [opcodeByte OP_DROP]

||| Get contract size in bytes
public export
contractSize : Contract -> Nat
contractSize c = length (compileContract c)

-- =============================================================================
-- Contract Builder DSL
-- =============================================================================

export infixl 5 @@

||| Add method operator
public export
(@@) : Contract -> Method -> Contract
(@@) c m = addMethod m c

||| Build a contract with fluent syntax
||| Example:
|||   contract "MyContract"
|||     @@ method "create" 0x01 createScript
|||     @@ method "update" 0x02 updateScript
public export
contract : String -> Contract
contract = emptyContract

-- =============================================================================
-- State Variable Abstraction
-- =============================================================================

||| State variable descriptor (for documentation and off-chain use)
public export
record StateVar where
  constructor MkStateVar
  varName : String
  varType : StackElem
  offset : Nat           -- Byte offset in state blob
  size : Nat             -- Size in bytes

||| Contract with state schema
public export
record StatefulContract where
  constructor MkStatefulContract
  baseContract : Contract
  stateVars : List StateVar
  stateSize : Nat        -- Total state blob size

||| Define a state variable
public export
stateVar : String -> StackElem -> Nat -> Nat -> StateVar
stateVar = MkStateVar

-- =============================================================================
-- Common Contract Patterns
-- =============================================================================

||| Simple signature-guarded contract
||| Single method: just verify signature
public export
simpleP2PKContract : Vect 33 Int -> Contract
simpleP2PKContract pubkey =
  let script : Script (SAny :: []) (SBool :: [])
      script = p2pk pubkey
  in contract "SimpleP2PK"
     @@ method "spend" 0x01 script

||| Time-locked contract
||| Can only spend after block height
public export
timelockContract : Int -> Vect 33 Int -> Contract
timelockContract locktime pubkey =
  let script : Script (SAny :: []) (SBool :: [])
      script = checkLockTime locktime >> OpDrop >> p2pk pubkey
  in contract "Timelock"
     @@ method "spend" 0x01 script

-- =============================================================================
-- Contract Introspection
-- =============================================================================

||| List all method names
public export
methodNames : Contract -> List String
methodNames c = map name c.methods

||| Find method by name
public export
findMethod : String -> Contract -> Maybe Method
findMethod n c = find (\m => m.name == n) c.methods

||| Find method by selector
public export
findMethodBySelector : Int -> Contract -> Maybe Method
findMethodBySelector sel c = find (\m => m.selector == sel) c.methods

-- =============================================================================
-- Contract Verification Helpers
-- =============================================================================

||| Check all selectors are unique
public export
uniqueSelectors : Contract -> Bool
uniqueSelectors c =
  let sels = map selector c.methods
  in length sels == length (nub sels)

||| Check all method names are unique
public export
uniqueNames : Contract -> Bool
uniqueNames c =
  let names = map name c.methods
  in length names == length (nub names)

||| Validate contract structure
public export
validateContract : Contract -> Either String ()
validateContract c =
  if not (uniqueSelectors c)
    then Left "Duplicate method selectors"
  else if not (uniqueNames c)
    then Left "Duplicate method names"
  else if length c.methods == 0
    then Left "Contract has no methods"
  else Right ()
