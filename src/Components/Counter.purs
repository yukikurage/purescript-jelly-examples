module Components.Counter where

import Prelude

import Components.PopIn (popIn)
import Contexts (Contexts)
import Contexts.ColorMode (mergeColorScheme, useColorScheme)
import Data.Tuple.Nested ((/\))
import Jelly.Data.Props (classes, on)
import Jelly.HTML (Component, elEmpty, text)
import Jelly.Hooks.UseState (useState)
import Utils (box, button)

counter :: Component Contexts
counter = do
  colorScheme <- useColorScheme
  count /\ modifyCount <- useState 0

  popIn $ box
    [ classes
        [ pure "h-24 w-24 flex flex-col justify-center items-center relative" ]
    ]
    do
      box
        [ classes
            [ pure
                "h-24 w-24 -rotate-0 hover:rotate-0 transition-all absolute origin-center rounded-md"
            , colorScheme <#> mergeColorScheme >>> _.highlight
            ]
        ]
        elEmpty
      button
        [ classes
            [ pure
                "h-24 w-24 hover:rotate-[20deg] shadow transition-all absolute origin-center rounded-md"
            , colorScheme <#> mergeColorScheme >>> _.reverse
            ]
        , on "click" \_ -> modifyCount (_ + 1)
        ]
        elEmpty
      box
        [ classes
            [ pure
                "flex justify-center items-center text-3xl z-20 relative pointer-events-none transition-colors"
            , colorScheme <#> mergeColorScheme >>> _.reverse
            ]
        ] $ text $ show <$> count
