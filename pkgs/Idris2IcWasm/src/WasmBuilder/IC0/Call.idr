||| IC0 Inter-Canister Call API
|||
||| Low-level FFI bindings for making calls to other canisters.
||| Uses IC System API: ic0_call_new, ic0_call_perform, etc.
|||
||| Example:
|||   result <- callCanister targetId "methodName" candid
module WasmBuilder.IC0.Call

import Data.List

%default covering

-- =============================================================================
-- IC0 System API FFI (imported from WASM environment)
-- =============================================================================

||| ic0.call_new - Create new inter-canister call
||| @calleeSrc  Pointer to callee principal bytes
||| @calleeSize Size of callee principal
||| @nameSrc    Pointer to method name
||| @nameSize   Size of method name
||| @replyFun   Reply callback function index (table)
||| @replyEnv   Reply callback environment
||| @rejectFun  Reject callback function index
||| @rejectEnv  Reject callback environment
export
%foreign "C:ic0_call_new,env"
prim__callNew : Int32 -> Int32 -> Int32 -> Int32 ->
                Int32 -> Int32 -> Int32 -> Int32 -> PrimIO ()

||| ic0.call_data_append - Append data to call payload
||| @src  Pointer to data
||| @size Size of data
export
%foreign "C:ic0_call_data_append,env"
prim__callDataAppend : Int32 -> Int32 -> PrimIO ()

||| ic0.call_cycles_add128 - Add cycles to call
||| @high High 64 bits of cycles
||| @low  Low 64 bits of cycles
export
%foreign "C:ic0_call_cycles_add128,env"
prim__callCyclesAdd128 : Bits64 -> Bits64 -> PrimIO ()

||| ic0.call_perform - Execute the call
||| Returns 0 on success, error code otherwise
export
%foreign "C:ic0_call_perform,env"
prim__callPerform : PrimIO Int32

||| ic0.msg_arg_data_size - Get response data size
export
%foreign "C:ic0_msg_arg_data_size,env"
prim__msgArgDataSize : PrimIO Int32

||| ic0.msg_arg_data_copy - Copy response data to buffer
||| @dst    Destination pointer
||| @offset Offset in response
||| @size   Number of bytes to copy
export
%foreign "C:ic0_msg_arg_data_copy,env"
prim__msgArgDataCopy : Int32 -> Int32 -> Int32 -> PrimIO ()

-- =============================================================================
-- Call State Management (Global buffers in C)
-- =============================================================================

||| Write byte to call buffer (callee ID or method name)
export
%foreign "C:ic_call_write_byte,libic0_call"
prim__callWriteByte : Int32 -> Int32 -> Int32 -> PrimIO ()

||| Get call buffer pointer
export
%foreign "C:ic_call_get_ptr,libic0_call"
prim__callGetPtr : Int32 -> Int32

||| Set call buffer length
export
%foreign "C:ic_call_set_len,libic0_call"
prim__callSetLen : Int32 -> Int32 -> PrimIO ()

||| Get response buffer pointer
export
%foreign "C:ic_call_response_ptr,libic0_call"
prim__callResponsePtr : Int32

||| Get response length
export
%foreign "C:ic_call_response_len,libic0_call"
prim__callResponseLen : PrimIO Int32

||| Get response byte at index
export
%foreign "C:ic_call_response_byte,libic0_call"
prim__callResponseByte : Int32 -> PrimIO Int32

||| Get call status (0=idle, 1=pending, 2=success, -1=error)
export
%foreign "C:ic_call_status,libic0_call"
prim__callStatus : PrimIO Int32

||| Set call status
export
%foreign "C:ic_call_set_status,libic0_call"
prim__callSetStatus : Int32 -> PrimIO ()

-- =============================================================================
-- Buffer IDs
-- =============================================================================

||| Buffer for callee principal
public export
BUFFER_CALLEE : Int32
BUFFER_CALLEE = 0

||| Buffer for method name
public export
BUFFER_METHOD : Int32
BUFFER_METHOD = 1

||| Buffer for request payload
public export
BUFFER_PAYLOAD : Int32
BUFFER_PAYLOAD = 2

-- =============================================================================
-- Call Status Type
-- =============================================================================

||| Status of an async call
public export
data CallStatus
  = CallIdle       -- 0: Not started
  | CallPending    -- 1: Waiting for response
  | CallSuccess    -- 2: Completed successfully
  | CallFailed     -- -1: Call failed or rejected

export
Show CallStatus where
  show CallIdle = "Idle"
  show CallPending = "Pending"
  show CallSuccess = "Success"
  show CallFailed = "Failed"

||| Convert status code to CallStatus
export
statusFromCode : Int32 -> CallStatus
statusFromCode 0 = CallIdle
statusFromCode 1 = CallPending
statusFromCode 2 = CallSuccess
statusFromCode _ = CallFailed

-- =============================================================================
-- High-Level API
-- =============================================================================

||| Write bytes to a call buffer
export
writeBuffer : Int32 -> List Bits8 -> IO ()
writeBuffer bufId bytes = go 0 bytes
  where
    go : Int32 -> List Bits8 -> IO ()
    go _ [] = primIO $ prim__callSetLen bufId (cast $ length bytes)
    go idx (b :: bs) = do
      primIO $ prim__callWriteByte bufId idx (cast b)
      go (idx + 1) bs

||| Write string to a call buffer (for method names)
export
writeBufferString : Int32 -> String -> IO ()
writeBufferString bufId s = writeBuffer bufId (map cast $ unpack s)

||| Get current call status
export
getCallStatus : IO CallStatus
getCallStatus = do
  code <- primIO prim__callStatus
  pure (statusFromCode code)

||| Set call status
export
setCallStatus : CallStatus -> IO ()
setCallStatus CallIdle = primIO $ prim__callSetStatus 0
setCallStatus CallPending = primIO $ prim__callSetStatus 1
setCallStatus CallSuccess = primIO $ prim__callSetStatus 2
setCallStatus CallFailed = primIO $ prim__callSetStatus (-1)

||| Read response bytes
export
readResponse : IO (List Bits8)
readResponse = do
  len <- primIO prim__callResponseLen
  go 0 (cast len) []
  where
    go : Int32 -> Nat -> List Bits8 -> IO (List Bits8)
    go _ Z acc = pure (reverse acc)
    go idx (S n) acc = do
      b <- primIO $ prim__callResponseByte idx
      go (idx + 1) n (cast b :: acc)

||| Initiate an inter-canister call
||| @calleeId   Principal bytes of target canister
||| @methodName Method name to call
||| @payload    Candid-encoded request payload
||| Returns True if call was initiated successfully
export
initiateCall : (calleeId : List Bits8) -> (methodName : String) -> (payload : List Bits8) -> IO Bool
initiateCall calleeId methodName payload = do
  -- Write callee ID to buffer
  writeBuffer BUFFER_CALLEE calleeId
  -- Write method name to buffer
  writeBufferString BUFFER_METHOD methodName
  -- Write payload to buffer
  writeBuffer BUFFER_PAYLOAD payload

  -- Set status to pending
  setCallStatus CallPending

  -- Create the call
  let calleePtr = prim__callGetPtr BUFFER_CALLEE
  let calleeLen = cast $ length calleeId
  let methodPtr = prim__callGetPtr BUFFER_METHOD
  let methodLen = cast $ length methodName

  primIO $ prim__callNew calleePtr calleeLen methodPtr methodLen 0 0 0 0

  -- Append payload
  let payloadPtr = prim__callGetPtr BUFFER_PAYLOAD
  let payloadLen = cast $ length payload
  primIO $ prim__callDataAppend payloadPtr payloadLen

  -- Perform the call
  result <- primIO prim__callPerform
  pure (result == 0)

||| Empty Candid payload (DIDL + 0 types + 0 args)
export
emptyCandid : List Bits8
emptyCandid = [0x44, 0x49, 0x44, 0x4C, 0x00, 0x00]  -- "DIDL" + 0x00 + 0x00
