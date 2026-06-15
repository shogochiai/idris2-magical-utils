module Main

import RN.Node
import RN.Runtime

data Msg = Tapped

record Model where
  constructor MkModel
  count : Nat

update : Msg -> Model -> Model
update Tapped m = { count := S m.count } m

view : Model -> Node Msg
view m = view' [testID "root"]
  [ text [] "Hello from Idris2 on React Native"
  , text [] ("tapped: " ++ show m.count)
  , button [onPress Tapped] "Tap me"
  ]
  where view' = RN.Node.view

main : IO ()
main = runMVU update Main.view Tapped (MkModel 0)
