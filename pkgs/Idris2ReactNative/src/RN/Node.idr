||| A dom-mvc-shaped UI tree for React Native.
|||
||| Design note (the "don't reproduce React in the type system" lesson):
||| idris2-dom-mvc's `Text.HTML.Node.Node` is pure data, but it is indexed by
||| `HTMLTag tag` — a type-level proof tied to *HTML* tags (div/span/...).
||| React Native primitives (View/Text/ScrollView/...) are not HTML tags, so
||| reusing that index would force RN components into an HTML straitjacket.
|||
||| Instead we keep the dom-mvc *shape* — `El`/`Text`/`Empty`, parameterised by
||| the event type `e`, with attributes and children — but identify components
||| by plain `String` names and pass props as plain key/value pairs. The React
||| element creation, prop translation, and event wiring all live behind the
||| `react-native:` FFI boundary (see `RN.Runtime`). This is the same pragmatic
||| split TheWorldUi used: the UI *tree* is typed Idris2; the React/runtime
||| edge is FFI.
module RN.Node

%default total

||| An attribute / prop on a React Native component.
|||
||| `Prop` carries a static string prop (e.g. style key, text value), and
||| `Handler` carries an event-producing callback (e.g. onPress -> e). The
||| renderer turns `Handler name f` into a JS function that feeds `f`'s event
||| back into the MVU dispatch loop.
public export
data Attr : (e : Type) -> Type where
  ||| A plain string-valued prop, e.g. `Prop "style" "title"`.
  Prop    : (name : String) -> (value : String) -> Attr e
  ||| An event handler, e.g. `Handler "onPress" Tapped`.
  Handler : (name : String) -> (event : e) -> Attr e
  ||| A string-carrying handler, e.g. `StrHandler "onChangeText" EditText`.
  ||| The renderer feeds the JS argument (the new text) through the function.
  StrHandler : (name : String) -> (mk : String -> e) -> Attr e

||| Map the event type of an attribute (functorial in `e`).
public export
mapAttr : (a -> b) -> Attr a -> Attr b
mapAttr _ (Prop n v)       = Prop n v
mapAttr f (Handler n e)    = Handler n (f e)
mapAttr f (StrHandler n g) = StrHandler n (f . g)

||| A React Native UI node, parameterised by the event type it can emit.
|||
||| `El comp attrs children` is a React element created from the RN component
||| named `comp` (e.g. "View", "Text", "Button"). `Str` is a text leaf.
||| `Empty` renders nothing.
public export
data Node : (e : Type) -> Type where
  El    : (comp : String) -> List (Attr e) -> List (Node e) -> Node e
  Str   : String -> Node e
  Empty : Node e

||| Map the event type of a node (functorial in `e`).
public export
mapNode : (a -> b) -> Node a -> Node b
mapNode f (El comp attrs kids) = El comp (map (mapAttr f) attrs) (assert_total $ map (mapNode f) kids)
mapNode _ (Str s)              = Str s
mapNode _ Empty                = Empty

-- =============================================================================
-- dom-mvc-style smart constructors (familiar authoring surface)
-- =============================================================================

||| A generic container component (`View`).
public export
view : List (Attr e) -> List (Node e) -> Node e
view = El "View"

||| A text component (`Text`). Children are text leaves.
public export
text : List (Attr e) -> String -> Node e
text attrs s = El "Text" attrs [Str s]

||| A button component (`Button`). RN's `Button` takes a `title` prop.
public export
button : List (Attr e) -> String -> Node e
button attrs title = El "Button" (Prop "title" title :: attrs) []

||| A scrollable container (`ScrollView`).
public export
scroll : List (Attr e) -> List (Node e) -> Node e
scroll = El "ScrollView"

||| A single-line text input (`TextInput`). Pair with `onChangeText`/`value`.
public export
textInput : List (Attr e) -> Node e
textInput attrs = El "TextInput" attrs []

||| A modern tappable surface (`Pressable`) — preferred over `Button` for
||| custom-styled, Claude-app-like rows and buttons.
public export
pressable : List (Attr e) -> List (Node e) -> Node e
pressable = El "Pressable"

||| A loading spinner (`ActivityIndicator`).
public export
spinner : List (Attr e) -> Node e
spinner attrs = El "ActivityIndicator" attrs []

||| A safe-area container (`SafeAreaView`) — respects notches/status bars.
public export
safeArea : List (Attr e) -> List (Node e) -> Node e
safeArea = El "SafeAreaView"

||| An image (`Image`); pass a `source` URI prop.
public export
image : List (Attr e) -> Node e
image attrs = El "Image" attrs []

-- =============================================================================
-- Attribute helpers
-- =============================================================================

||| An `onPress` handler producing event `e`.
public export
onPress : e -> Attr e
onPress = Handler "onPress"

||| An `onChangeText` handler: the new text is fed through `mk`.
public export
onChangeText : (String -> e) -> Attr e
onChangeText = StrHandler "onChangeText"

||| The controlled `value` of a TextInput.
public export
value : String -> Attr e
value = Prop "value"

||| A `placeholder` prop for a TextInput.
public export
placeholder : String -> Attr e
placeholder = Prop "placeholder"

||| A `testID` prop (handy for driving the app from a test harness).
public export
testID : String -> Attr e
testID = Prop "testID"

||| A React `key` — REQUIRED on every element rendered from a list/`map`, so
||| React can reconcile array children (without it RN-Paper crashes on mount:
||| "Trying to add unknown view tag").
public export
key : String -> Attr e
key = Prop "key"
