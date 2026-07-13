||| Ergonomic styling for RN.Node.
|||
||| React Native styles are plain objects of camelCase keys. We model a style as
||| a list of (key, JSON-value) pairs and serialise it to the JSON string that
||| the shim's `setProp` parses into `props.style`. Number values are emitted
||| bare; string values are quoted. This keeps the typed Idris layer ergonomic
||| while leaving the actual RN style application at the JS/FFI edge.
module RN.Style

import RN.Node
import Data.String

%default total

||| A single style declaration: key plus a pre-rendered JSON value fragment.
public export
record StyleDecl where
  constructor MkStyle
  key   : String
  value : String  -- raw JSON fragment, e.g. "12", "\"#fff\"", "\"center\""

||| String-valued style (quoted), e.g. `s "color" "#fff"`.
public export
s : String -> String -> StyleDecl
s k v = MkStyle k ("\"" ++ v ++ "\"")

||| Number-valued style (bare), e.g. `n "padding" 12`.
public export
n : String -> Int -> StyleDecl
n k v = MkStyle k (show v)

||| Render a list of style decls to a JSON object string.
export
renderStyle : List StyleDecl -> String
renderStyle decls =
  "{" ++ joinBy "," (map (\d => "\"" ++ d.key ++ "\":" ++ d.value) decls) ++ "}"

||| A `style` attribute built from style decls.
public export
style : List StyleDecl -> Attr e
style decls = Prop "style" (renderStyle decls)

-- =============================================================================
-- Common style fragments (a Gemini-like light design system:
-- white base, one blue pinpoint accent)
-- =============================================================================

||| App palette.
public export
bgColor : String
bgColor = "#ffffff"          -- white base

public export
surfaceColor : String
surfaceColor = "#f7f8fa"     -- card / surface (very light gray)

public export
accentColor : String
accentColor = "#1a73e8"      -- Gemini blue pinpoint accent

public export
textColor : String
textColor = "#1f1f1f"        -- near-black text

public export
mutedColor : String
mutedColor = "#5f6368"       -- Google gray secondary text

||| Hairline divider / border on the light base.
public export
borderColor : String
borderColor = "#e3e5e8"

||| Text / icon colour to place ON the accent (white on blue).
public export
onAccentColor : String
onAccentColor = "#ffffff"

||| Flex column container filling available space.
public export
fill : List StyleDecl
fill = [n "flex" 1]
