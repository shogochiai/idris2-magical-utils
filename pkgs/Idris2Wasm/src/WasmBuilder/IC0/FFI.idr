||| IC FFI Bridge - Idris2 bindings
|||
||| Generic C ↔ Idris2 communication for IC canisters.
||| Use these instead of declaring %foreign yourself.
|||
||| Example usage:
|||   main : IO ()
|||   main = do
|||     cmd <- getArg 0
|||     result <- processCommand cmd
|||     setResult result
module WasmBuilder.IC0.FFI

%default covering

-- =============================================================================
-- Argument/Result Bridge
-- =============================================================================

||| Get argument passed from C entry point
||| @index Argument index (0-7)
export
%foreign "C:ic_ffi_get_arg,libic0"
prim__getArg : Int -> Int

||| Get argument (IO wrapper)
export
getArg : Int -> IO Int
getArg idx = pure (prim__getArg idx)

||| Set result to return to C entry point
export
%foreign "C:ic_ffi_set_result,libic0"
prim__setResult : Int -> PrimIO ()

||| Set result (IO wrapper)
export
setResult : Int -> IO ()
setResult val = primIO $ prim__setResult val

-- =============================================================================
-- Candid Buffer (Idris2 → C)
-- =============================================================================

||| Write byte to Candid buffer
export
%foreign "C:ic_candid_write_byte,libic0"
prim__candidWriteByte : Int -> Int -> PrimIO ()

||| Write byte to Candid buffer at index
export
candidWriteByte : Int -> Int -> IO ()
candidWriteByte idx byte = primIO $ prim__candidWriteByte idx byte

||| Set Candid buffer length
export
%foreign "C:ic_candid_set_len,libic0"
prim__candidSetLen : Int -> PrimIO ()

||| Set Candid buffer length
export
candidSetLen : Int -> IO ()
candidSetLen len = primIO $ prim__candidSetLen len

||| Clear Candid buffer
export
%foreign "C:ic_candid_clear,libic0"
prim__candidClear : PrimIO ()

||| Clear Candid buffer
export
candidClear : IO ()
candidClear = primIO prim__candidClear

||| Write bytes to Candid buffer
export
candidWriteBytes : List Int -> IO ()
candidWriteBytes bytes = do
  candidClear
  go 0 bytes
  candidSetLen (cast $ length bytes)
  where
    go : Int -> List Int -> IO ()
    go _ [] = pure ()
    go idx (b :: bs) = do
      candidWriteByte idx b
      go (idx + 1) bs

-- =============================================================================
-- JSON Buffer (C → Idris2)
-- =============================================================================

||| Get JSON buffer length
export
%foreign "C:ic_json_get_len,libic0"
prim__jsonGetLen : PrimIO Int

||| Get JSON buffer length
export
jsonGetLen : IO Int
jsonGetLen = primIO prim__jsonGetLen

||| Get byte from JSON buffer
export
%foreign "C:ic_json_get_byte,libic0"
prim__jsonGetByte : Int -> PrimIO Int

||| Get byte from JSON buffer at index
export
jsonGetByte : Int -> IO Int
jsonGetByte idx = primIO $ prim__jsonGetByte idx

||| Read all bytes from JSON buffer
export
jsonReadBytes : IO (List Int)
jsonReadBytes = do
  len <- jsonGetLen
  go 0 len []
  where
    go : Int -> Int -> List Int -> IO (List Int)
    go idx len acc =
      if idx >= len
        then pure (reverse acc)
        else do
          byte <- jsonGetByte idx
          go (idx + 1) len (byte :: acc)

-- =============================================================================
-- String Buffer (Idris2 → C)
-- =============================================================================

||| Write byte to string buffer
export
%foreign "C:ic_str_write_byte,libic0"
prim__strWriteByte : Int -> Int -> PrimIO ()

||| Write byte to string buffer at index
export
strWriteByte : Int -> Int -> IO ()
strWriteByte idx byte = primIO $ prim__strWriteByte idx byte

||| Set string buffer length
export
%foreign "C:ic_str_set_len,libic0"
prim__strSetLen : Int -> PrimIO ()

||| Set string buffer length
export
strSetLen : Int -> IO ()
strSetLen len = primIO $ prim__strSetLen len

||| Write string to string buffer
export
strWrite : String -> IO ()
strWrite s = do
  let bytes = map cast (unpack s)
  go 0 bytes
  strSetLen (cast $ length bytes)
  where
    go : Int -> List Int -> IO ()
    go _ [] = pure ()
    go idx (b :: bs) = do
      strWriteByte idx b
      go (idx + 1) bs
