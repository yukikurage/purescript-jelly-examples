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
            "relative w-56 mt-6 h-4 z-10 bg-fuchsia-600 flex flex-row justify-center items-end rounded-t-md"
        ]
    ]
    [ box
        [ classes
            [ pure
                "relative text-5xl z-20 font-AlfaSlabOne font-extrabold text-white"
            ]
        ]
        [ text $ pure "JELLY" ]
    ]
