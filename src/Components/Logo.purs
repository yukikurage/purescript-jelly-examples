module Components.Logo where

import Prelude

import Jelly.Data.Props (classes)
import Jelly.HTML (Component, text)
import Utils (box)

logo :: forall r. Component r
logo = do
  box
    [ classes
        [ pure
            "relative w-56 h-8 z-10 overflow-hidden bg-white rounded-md shadow-md"
        ]
    ]
    [ box
        [ classes
            [ pure
                "relative text-6xl z-20 font-serif font-extrabold text-slate-900 -top-4 left-3"
            ]
        ]
        [ text $ pure "JELLY" ]
    ]
