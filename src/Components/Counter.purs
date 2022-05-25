module Components.Counter where

import Prelude

import Components.PopIn (popIn)
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

counter :: Component Unit
counter = do
  count /\ modifyCount <- newJelly 0

  popIn
    [ box
        [ classes
            [ pure
                "h-16 w-16 flex flex-col justify-center items-center relative"
            ]
        ]
        [ button
            [ classes
                [ pure
                    "bg-white h-16 w-16 rotate-6 hover:rotate-0 transition-all absolute origin-center"
                ]
            , on "click" \_ -> modifyCount (_ + 1)
            ]
            []
        , box
            [ classes
                [ pure
                    "flex justify-center items-center text-slate-900 text-3xl z-20 relative pointer-events-none"
                ]
            ]
            [ text $ show <$> count ]
        ]
    ]
