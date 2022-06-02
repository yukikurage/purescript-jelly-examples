module Components.Counter where

import Prelude

import Contexts (Contexts)
import Contexts.ColorMode (mergeColorScheme, useColorScheme)
import Hooks.UsePopIn (usePopIn)
import Jelly.Data.Jelly (modify, read)
import Jelly.Data.Props (classes, on)
import Jelly.HTML (Component, elEmpty, text)
import Jelly.Hooks.UseState (useState)
import Utils (box, button)

counter :: Component Contexts
counter = do
  colorScheme <- useColorScheme
  count <- useState 0

  popIn <- usePopIn

  box
    [ classes
        [ pure "h-24 w-24 flex flex-col justify-center items-center relative"
        , popIn
        ]
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
        , on "click" \_ -> modify count (_ + 1)
        ]
        elEmpty
      box
        [ classes
            [ pure
                "flex justify-center items-center text-3xl z-20 relative pointer-events-none transition-colors"
            , colorScheme <#> mergeColorScheme >>> _.reverse
            ]
        ] $ text $ show <$> read count
