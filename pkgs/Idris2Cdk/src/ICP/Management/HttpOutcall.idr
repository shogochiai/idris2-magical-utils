||| ICP Management Canister - HTTP Outcall
|||
||| This module provides typed access to the ICP Management Canister's
||| HTTP outcall functionality.
|||
||| The Management Canister (aaaaa-aa) provides system-level services including:
||| - HTTP outcalls to external services
||| - Threshold ECDSA signing
||| - Canister management (create, install, etc.)
|||
||| Reference: https://internetcomputer.org/docs/current/references/ic-interface-spec#ic-http_request
module ICP.Management.HttpOutcall

import ICP.IC0
import ICP.Candid.Types
import Data.List
import Data.String

import FRMonad.Graded
import FRMonad.Failure
import FRMonad.Evidence

%default total

--------------------------------------------------------------------------------
-- Management Canister ID
--------------------------------------------------------------------------------

||| The management canister's principal (aaaaa-aa)
public export
managementCanister : Principal
managementCanister = MkPrincipal []  -- Empty principal = management canister

--------------------------------------------------------------------------------
-- HTTP Request Types
--------------------------------------------------------------------------------

||| HTTP method
public export
data HttpMethod = GET | POST | HEAD

public export
Show HttpMethod where
  show GET  = "GET"
  show POST = "POST"
  show HEAD = "HEAD"

||| HTTP header
public export
record HttpHeader where
  constructor MkHttpHeader
  name  : String
  value : String

||| Transform context for response processing
public export
record TransformContext where
  constructor MkTransformContext
  function : String  -- Transform function name
  context  : List Bits8  -- Context bytes

||| HTTP request configuration
public export
record HttpRequest where
  constructor MkHttpRequest
  url              : String
  maxResponseBytes : Maybe Nat
  method           : HttpMethod
  headers          : List HttpHeader
  body             : Maybe (List Bits8)
  transform        : Maybe TransformContext

||| HTTP response from outcall
public export
record HttpResponse where
  constructor MkHttpResponse
  status  : Nat
  headers : List HttpHeader
  body    : List Bits8

--------------------------------------------------------------------------------
-- Transform Function Type
--------------------------------------------------------------------------------

||| Transform function signature
||| Takes raw response and transform context, returns transformed response
public export
TransformFunc : Type
TransformFunc = HttpResponse -> List Bits8 -> HttpResponse

--------------------------------------------------------------------------------
-- Request Builder (Fluent API)
--------------------------------------------------------------------------------

||| Create a new GET request
public export
httpGet : String -> HttpRequest
httpGet url = MkHttpRequest url Nothing GET [] Nothing Nothing

||| Create a new POST request
public export
httpPost : String -> List Bits8 -> HttpRequest
httpPost url reqBody = MkHttpRequest url Nothing POST [] (Just reqBody) Nothing

||| Add header to request
public export
withHeader : String -> String -> HttpRequest -> HttpRequest
withHeader n v req =
  MkHttpRequest req.url req.maxResponseBytes req.method
                (MkHttpHeader n v :: req.headers) req.body req.transform

||| Set max response bytes
public export
withMaxResponseBytes : Nat -> HttpRequest -> HttpRequest
withMaxResponseBytes n req =
  MkHttpRequest req.url (Just n) req.method req.headers req.body req.transform

||| Set transform function
public export
withTransform : String -> List Bits8 -> HttpRequest -> HttpRequest
withTransform func ctx req =
  MkHttpRequest req.url req.maxResponseBytes req.method req.headers req.body
                (Just (MkTransformContext func ctx))

||| Common headers
public export
withContentType : String -> HttpRequest -> HttpRequest
withContentType ct = withHeader "Content-Type" ct

public export
withJsonContentType : HttpRequest -> HttpRequest
withJsonContentType = withContentType "application/json"

--------------------------------------------------------------------------------
-- Candid Encoding for HTTP Types
--------------------------------------------------------------------------------

public export
Candidable HttpMethod where
  candidType = CtVariant
    [ MkFieldType (hashFieldName "get") (Just "get") CtNull
    , MkFieldType (hashFieldName "post") (Just "post") CtNull
    , MkFieldType (hashFieldName "head") (Just "head") CtNull
    ]
  toCandid GET  = CVariant (hashFieldName "get") CNull
  toCandid POST = CVariant (hashFieldName "post") CNull
  toCandid HEAD = CVariant (hashFieldName "head") CNull
  fromCandid (CVariant h CNull) =
    if h == hashFieldName "get" then Just GET
    else if h == hashFieldName "post" then Just POST
    else if h == hashFieldName "head" then Just HEAD
    else Nothing
  fromCandid _ = Nothing

public export
Candidable HttpHeader where
  candidType = CtRecord
    [ MkFieldType (hashFieldName "name") (Just "name") CtText
    , MkFieldType (hashFieldName "value") (Just "value") CtText
    ]
  toCandid (MkHttpHeader n v) = CRecord
    [ (hashFieldName "name", CText n)
    , (hashFieldName "value", CText v)
    ]
  fromCandid (CRecord fields) = do
    n <- lookup (hashFieldName "name") fields >>= fromCandid
    v <- lookup (hashFieldName "value") fields >>= fromCandid
    Just (MkHttpHeader n v)
  fromCandid _ = Nothing

public export
Candidable TransformContext where
  candidType = CtRecord
    [ MkFieldType (hashFieldName "function") (Just "function") (CtFunc [CtRecord []] [CtRecord []] [Query])
    , MkFieldType (hashFieldName "context") (Just "context") (CtVec CtNat8)
    ]
  toCandid (MkTransformContext f c) = CRecord
    [ (hashFieldName "function", CText f)  -- Simplified
    , (hashFieldName "context", CBlob c)
    ]
  fromCandid _ = Nothing  -- Not typically decoded

--------------------------------------------------------------------------------
-- Cycles Cost
--------------------------------------------------------------------------------

||| Base cost for HTTP outcall (in cycles)
public export
httpOutcallBaseCost : Nat
httpOutcallBaseCost = 3000000  -- 3M cycles

||| Cost per byte of request
public export
httpOutcallRequestByteCost : Nat
httpOutcallRequestByteCost = 400  -- 400 cycles per byte

||| Cost per byte of response
public export
httpOutcallResponseByteCost : Nat
httpOutcallResponseByteCost = 800  -- 800 cycles per byte

||| Calculate estimated cycles for request
public export
estimateCycles : HttpRequest -> Nat
estimateCycles req =
  let urlLen = length (unpack req.url)
      bodyLen = maybe 0 length req.body
      requestSize = urlLen + bodyLen
      maxResponse = maybe 2000000 id req.maxResponseBytes  -- 2MB default
  in httpOutcallBaseCost
     + requestSize * httpOutcallRequestByteCost
     + maxResponse * httpOutcallResponseByteCost

--------------------------------------------------------------------------------
-- HTTP Outcall Execution
--------------------------------------------------------------------------------

||| Result of HTTP outcall
public export
data HttpOutcallResult
  = HttpSuccess HttpResponse
  | HttpError String

||| Execute HTTP outcall
||| This calls the management canister's http_request method
export
httpRequest : HttpRequest -> IO HttpOutcallResult
httpRequest req = do
  -- In real implementation:
  -- 1. Encode request to Candid
  -- 2. Call management canister (aaaaa-aa) method "http_request"
  -- 3. Attach sufficient cycles
  -- 4. Decode response from Candid
  --
  -- Placeholder implementation:
  pure (HttpError "Not implemented - requires ICP runtime")

--------------------------------------------------------------------------------
-- Common HTTP Patterns
--------------------------------------------------------------------------------

||| Fetch JSON from URL
export
fetchJson : String -> IO HttpOutcallResult
fetchJson url =
  httpRequest (httpGet url |> withHeader "Accept" "application/json")

||| Post JSON to URL
export
postJson : String -> String -> IO HttpOutcallResult
postJson url jsonBody =
  let bodyBytes = map (cast . ord) (unpack jsonBody)
  in httpRequest (httpPost url bodyBytes |> withJsonContentType)

--------------------------------------------------------------------------------
-- Response Helpers
--------------------------------------------------------------------------------

||| Check if response is successful (2xx)
public export
isSuccess : HttpResponse -> Bool
isSuccess resp = resp.status >= 200 && resp.status < 300

||| Get response body as string
public export
bodyAsString : HttpResponse -> String
bodyAsString resp = pack (map (chr . cast) resp.body)

||| Convert string to lowercase
strToLower : String -> String
strToLower s = pack (map toLower (unpack s))

||| Get header value by name (case-insensitive)
public export
getHeader : String -> HttpResponse -> Maybe String
getHeader n resp =
  let nameLower = strToLower n
  in map value $ find (\h => strToLower h.name == nameLower) resp.headers

--------------------------------------------------------------------------------
-- GFR Integration: Graded FR Monad wrappers for HttpOutcall
--------------------------------------------------------------------------------

||| Convert HttpOutcallResult to GFR with NetworkObligation
||| This is a pure conversion (no IO) for testing/mocking
public export
httpResultToGFR : HttpOutcallResult -> GFR [NetworkObligation] HttpResponse
httpResultToGFR (HttpSuccess resp) =
  GOk resp (mkEvidence Update "http" ("success: " ++ show resp.status))
httpResultToGFR (HttpError msg) =
  GFail (HttpError 0 msg) (mkEvidence Update "http" ("error: " ++ msg))

||| GFR-based HTTP request (for use in pure GFR computations)
||| Returns GFR [NetworkObligation] to track that network failures may occur
|||
||| Usage:
|||   result <- gHttpRequest req   -- in GFR [NetworkObligation] context
|||
||| To run, handle the NetworkObligation:
|||   handleNetwork retryHandler (gHttpRequest req)
public export
gHttpRequestPure : HttpOutcallResult -> GFR [NetworkObligation] HttpResponse
gHttpRequestPure = httpResultToGFR

||| GFR-based HTTP request with retry (pure version)
||| Retries failed requests according to config, accumulating evidence
public export
gHttpRequestWithRetry : RetryConfig -> HttpOutcallResult -> GFR [NetworkObligation] HttpResponse
gHttpRequestWithRetry cfg result = gretry cfg (httpResultToGFR result)

||| GFR-based HTTP request with default retry config
public export
gHttpRequestRetry : HttpOutcallResult -> GFR [NetworkObligation] HttpResponse
gHttpRequestRetry = gHttpRequestWithRetry networkRetryConfig

--------------------------------------------------------------------------------
-- Network failure handler combinators
--------------------------------------------------------------------------------

||| Create a retry handler for handleNetwork
||| This handler retries the failed operation with the given config
public export
retryHandler : RetryConfig -> (Fail, Evidence) -> GFR obs HttpResponse
retryHandler cfg (f, ev) =
  -- On retry exhaustion, we still fail but with accumulated evidence
  GFail f ev

||| Handler that returns a default response on failure
public export
defaultResponseHandler : HttpResponse -> (Fail, Evidence) -> GFR obs HttpResponse
defaultResponseHandler defaultResp (_, ev) =
  GOk defaultResp (combineEvidence ev (mkEvidence Update "http" "using default response"))

||| Handler that maps network errors to a specific failure
public export
mapNetworkError : (String -> Fail) -> (Fail, Evidence) -> GFR obs a
mapNetworkError mkFail (f, ev) =
  GFail (mkFail (show f)) ev

--------------------------------------------------------------------------------
-- Example usage patterns
--------------------------------------------------------------------------------

-- ||| Example: Fetch with retry and fallback
-- |||
-- ||| fetchWithFallback : HttpRequest -> HttpResponse -> GFR [] HttpResponse
-- ||| fetchWithFallback req fallback =
-- |||   handleNetwork (defaultResponseHandler fallback) $
-- |||   gHttpRequestRetry (HttpError "simulated")
-- |||
-- ||| Example: Chain multiple HTTP calls with obligation tracking
-- |||
-- ||| fetchBoth : HttpRequest -> HttpRequest -> GFR [NetworkObligation] (HttpResponse, HttpResponse)
-- ||| fetchBoth req1 req2 =
-- |||   gHttpRequestPure result1 >>>>= \resp1 =>
-- |||   gHttpRequestPure result2 >>>>= \resp2 =>
-- |||   gpure (resp1, resp2)
