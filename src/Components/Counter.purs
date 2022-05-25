module Components.Counter where

import Prelude

import Data.Tuple.Nested ((/\))
import Jelly.Data.Jelly (newJelly)
import Jelly.Data.Props (classes, on)
import Jelly.HTML (Component, text)
import Utils (box, button)

message :: Int -> String
message n = case n of
  0 -> "すべての始まり"
  1 -> "最初の一歩"
  2 -> "最小の素数"
  7 -> "ラッキーセブン！"
  11 -> "サッカーチーム"
  12 -> "時計が一周"
  18 -> "成人"
  x -> show x

counter :: forall r. Component r
counter = do
  count /\ modifyCount <- newJelly 0

  box [ classes [ pure "flex flex-col items-center gap-3" ] ]
    [ box [ classes [ pure "text-3xl text-white " ] ]
        [ text $ show <$> count ]
    , button
        [ classes
            [ pure
                "bg-white rounded-md h-10 w-16 text-3xl text-slate-900 hover:rounded-lg transition-all"
            ]
        , on "click" \_ -> modifyCount (_ + 1)
        ]
        []
    ]
