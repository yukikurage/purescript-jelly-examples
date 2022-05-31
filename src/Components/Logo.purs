module Components.Logo where

import Prelude

import Contexts (Contexts)
import Contexts.ColorMode (mergeColorScheme, useColorScheme)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component, text)
import Utils (box)

logo :: Component Contexts
logo = do
  colorScheme <- useColorScheme

  box
    [ classes
        [ pure
            "relative w-56 mt-6 h-5 z-10  flex flex-row justify-center items-end rounded-t-md transition-colors"
        , colorScheme <#> mergeColorScheme >>> _.highlight
        ]
    ]
    do
      box
        [ classes
            [ pure
                "relative text-5xl z-20 font-AlfaSlabOne font-extrabold transition-colors drop-shadow"
            ]
        ]
        do
          text $ pure "JELLY"
