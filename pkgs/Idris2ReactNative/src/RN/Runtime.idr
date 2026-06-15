||| React Native runtime bridge for the RN.Node UI tree.
|||
||| This is the RN analogue of idris2-dom-mvc's renderer: it walks a `Node e`
||| and builds a React element via `React.createElement`, wiring `Handler`
||| attributes back into a dispatch callback. All React/runtime interaction is
||| isolated behind `%foreign "react-native:..."` declarations, which the
||| react-native codegen resolves ahead of the "browser"/"javascript" fallbacks.
|||
||| The JS implementations live in `rn-runtime/idris-rn-shim.js`, injected by
||| the generated app entry (see `RN.Scaffold`).
module RN.Runtime

import RN.Node
import Data.IORef

%default covering

-- =============================================================================
-- Opaque handles into the JS runtime
-- =============================================================================

||| An opaque React element / props object / children array handle.
||| Represented on the JS side as a real React element or array.
export
data RElem : Type where [external]

||| An opaque mutable props builder.
export
data RProps : Type where [external]

-- =============================================================================
-- Primitive FFI (resolved by the `react-native` backend)
-- =============================================================================

%foreign "react-native:lambda: () => __idrisRN.newProps()"
prim__newProps : PrimIO RProps

%foreign "react-native:lambda: (p, k, v) => __idrisRN.setProp(p, k, v)"
prim__setProp : RProps -> String -> String -> PrimIO ()

%foreign "react-native:lambda: (p, k, cb) => __idrisRN.setHandler(p, k, cb)"
prim__setHandler : RProps -> String -> (() -> PrimIO ()) -> PrimIO ()

%foreign "react-native:lambda: (p, k, cb) => __idrisRN.setStrHandler(p, k, cb)"
prim__setStrHandler : RProps -> String -> (String -> PrimIO ()) -> PrimIO ()

%foreign "react-native:lambda: () => __idrisRN.newChildren()"
prim__newChildren : PrimIO RElem

%foreign "react-native:lambda: (arr, c) => __idrisRN.pushChild(arr, c)"
prim__pushChild : RElem -> RElem -> PrimIO ()

%foreign "react-native:lambda: (comp, p, kids) => __idrisRN.createElement(comp, p, kids)"
prim__createElement : String -> RProps -> RElem -> PrimIO RElem

%foreign "react-native:lambda: (s) => __idrisRN.textNode(s)"
prim__textNode : String -> PrimIO RElem

%foreign "react-native:lambda: () => __idrisRN.emptyNode()"
prim__emptyNode : PrimIO RElem

%foreign "react-native:lambda: (render) => __idrisRN.run(render)"
prim__run : (() -> PrimIO RElem) -> PrimIO ()

%foreign "react-native:lambda: () => __idrisRN.requestRerender()"
prim__requestRerender : PrimIO ()

-- =============================================================================
-- Idris-side renderer: Node e -> RElem
-- =============================================================================

||| Build the props object for a node's attributes, wiring handlers through
||| `dispatch`.
buildProps : (dispatch : e -> IO ()) -> List (Attr e) -> IO RProps
buildProps dispatch attrs = do
  p <- primIO prim__newProps
  go p attrs
  pure p
 where
  go : RProps -> List (Attr e) -> IO ()
  go _ [] = pure ()
  go p (Prop k v :: rest) = do
    primIO (prim__setProp p k v)
    go p rest
  go p (Handler k ev :: rest) = do
    primIO (prim__setHandler p k (\() => toPrim (dispatch ev)))
    go p rest
  go p (StrHandler k mk :: rest) = do
    primIO (prim__setStrHandler p k (\s => toPrim (dispatch (mk s))))
    go p rest

||| Render a `Node e` into a React element, dispatching events via `dispatch`.
export
renderNode : (dispatch : e -> IO ()) -> Node e -> IO RElem
renderNode dispatch (Str s) = primIO (prim__textNode s)
renderNode dispatch Empty   = primIO prim__emptyNode
renderNode dispatch (El comp attrs kids) = do
  p     <- buildProps dispatch attrs
  arr   <- primIO prim__newChildren
  renderChildren arr kids
  primIO (prim__createElement comp p arr)
 where
  renderChildren : RElem -> List (Node e) -> IO ()
  renderChildren _   []          = pure ()
  renderChildren arr (k :: rest) = do
    c <- renderNode dispatch k
    primIO (prim__pushChild arr c)
    renderChildren arr rest

-- =============================================================================
-- Minimal MVU loop (Elm/dom-mvc style)
-- =============================================================================

||| Run a Model-View-Update application on React Native.
|||
||| @ update  pure state transition
||| @ view    render the current model to a Node tree
||| @ initEv  the initial event (e.g. `Init`)
||| @ initSt  the initial model
|||
||| The current model is held in an IORef. Each dispatched event runs `update`,
||| stores the new model, and asks the runtime to re-render (which re-invokes
||| the render thunk, calling `view` on the fresh model).
export
runMVU :
     {0 e, s : Type}
  -> (update : e -> s -> s)
  -> (view   : s -> Node e)
  -> (initEv : e)
  -> (initSt : s)
  -> IO ()
runMVU update view initEv initSt = do
  ref <- newIORef (update initEv initSt)
  let dispatch : e -> IO ()
      dispatch ev = do
        st <- readIORef ref
        writeIORef ref (update ev st)
        primIO prim__requestRerender
  let render : () -> PrimIO RElem
      render = \() => toPrim (do st <- readIORef ref
                                 renderNode dispatch (view st))
  primIO (prim__run render)

-- =============================================================================
-- Cmd-bearing MVU (dom-mvc style: update is pure, side effects are Commands)
-- =============================================================================

||| A command is a side-effecting action that may dispatch follow-up events.
||| It receives the dispatch function so async results (e.g. a canister query
||| completing) can be fed back into the loop as new events.
|||
||| This mirrors idris2-dom-mvc's `Cmd e`: pure `update` decides the next state,
||| `display` decides what effects to run, and effects loop back via `dispatch`.
public export
Cmd : Type -> Type
Cmd e = (e -> IO ()) -> IO ()

||| The no-op command.
public export
none : Cmd e
none = \_ => pure ()

||| Run several commands in sequence.
public export
batch : List (Cmd e) -> Cmd e
batch cmds = \dispatch => for_ cmds (\c => c dispatch)

||| A command that fires a single event immediately (useful as an init kick).
public export
fire : e -> Cmd e
fire ev = \dispatch => dispatch ev

||| Run a Model-View-Update loop with commands.
|||
||| @ update   pure state transition
||| @ display  effects to run for a given event/state (canister calls, etc.)
||| @ view     render the current model to a Node tree
||| @ initEv   the initial event (e.g. `Init`)
||| @ initSt   the initial model
|||
||| Flow per event: store `update ev st`, re-render, then run `display ev st'`.
||| Commands call `dispatch` with follow-up events (e.g. `GotInstances xs`),
||| which re-enter the same loop.
export
runMVUc :
     {0 e, s : Type}
  -> (update  : e -> s -> s)
  -> (display : e -> s -> Cmd e)
  -> (view    : s -> Node e)
  -> (initEv  : e)
  -> (initSt  : s)
  -> IO ()
runMVUc update display view initEv initSt = do
  ref <- newIORef initSt
  -- `dispatch` is recursive through the IORef-held closure; define it via a
  -- helper that re-reads state each time.
  dispatchRef <- newIORef (the (e -> IO ()) (\_ => pure ()))
  let dispatch : e -> IO ()
      dispatch ev = do
        st <- readIORef ref
        let st' = update ev st
        writeIORef ref st'
        primIO prim__requestRerender
        -- run the effects for this transition, looping results back in
        d <- readIORef dispatchRef
        display ev st' d
  writeIORef dispatchRef dispatch
  let render : () -> PrimIO RElem
      render = \() => toPrim (do st <- readIORef ref
                                 renderNode dispatch (view st))
  primIO (prim__run render)
  -- kick off the initial event (runs update + display for initEv)
  dispatch initEv
