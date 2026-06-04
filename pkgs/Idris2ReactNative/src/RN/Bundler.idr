||| RN.Bundler — typed surface for the ERC-4337 bundler-as-canister.
|||
||| The DAO3.0 Wallet (and any RN app) submits a UserOp by calling the
||| GlobalRegistry canister's bundler methods, NOT a hand-written
||| `eth_sendUserOperation` HTTP call. This module is the ONLY thing the app's
||| MVU layer talks to: a `%foreign "react-native:..."` declaration backed by the
||| generated `global.__dao3Bundler` bridge (emitted by RN.Scaffold.bundlerBridgeJs).
||| Per the project rule, the raw runtime (the canister actor + EVM broadcast glue)
||| is a GENERATED artifact, not hand-written JS.
|||
||| sendToken: given the app-built UserOp JSON, the bridge drives the full
||| canister pipeline — submitUserOp (handleOps calldata) → signEvmTxHash /
||| signedEvmTx-resume loop (t-ECDSA + in-canister recovery) → assembleSignedTx →
||| eth_sendRawTransaction — and returns the broadcast tx hash (or an error).
module RN.Bundler

import Data.String

%default total

||| Result of a bundled send: the Base Mainnet tx hash, or an error string.
public export
data SendResult = SendOk String | SendErr String

public export
Show SendResult where
  show (SendOk h)  = "ok:" ++ h
  show (SendErr e) = "err:" ++ e

%foreign "react-native:lambda: (op, credId, cb) => __dao3Bundler.sendToken(op, credId, (s) => cb(s)())"
prim__sendToken : String -> String -> (String -> PrimIO ()) -> PrimIO ()

||| Submit a UserOp through the canister bundler. `userOpJson` is the app-built
||| EntryPoint v0.7 op (sender, nonce, initCode, callData, gas fields,
||| paymasterAndData, signature-placeholder) plus the beneficiary; `credId` is the
||| bound passkey credential id. The bridge fetches the canonical userOpHash,
||| runs the WebAuthn assertion over it (pinned to credId), sets op.signature to
||| the assertion blob, then submits + broadcasts. The callback gets the result.
export
sendToken : (userOpJson : String) -> (credId : String) -> (SendResult -> IO ()) -> IO ()
sendToken op credId k = primIO (prim__sendToken op credId (\s => toPrim (handle s)))
  where
    handle : String -> IO ()
    handle s =
      let t = trim s in
      if isPrefixOf "ok:" t then k (SendOk (trim (substr 3 (length t) t)))
      else if isPrefixOf "err:" t then k (SendErr (trim (substr 4 (length t) t)))
      else k (SendErr t)
