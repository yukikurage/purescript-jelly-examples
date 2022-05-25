module Main where

import Prelude

import Components.Counter (counter)
import Components.Logo (logo)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Jellies.TypingEffectJelly (typingEffectJelly)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component, text)
import Jelly.RunComponent (runComponent)
import Utils (box)

main :: Effect Unit
main = do
  runComponent unit root

root :: forall r. Component r
root = do
  jellyIs /\ isComplete <- typingEffectJelly
    "Jelly is easy way to create interactive web apps"
  box
    [ classes
        [ pure
            "h-screen w-screen relative bg-slate-900 overflow-hidden flex flex-col"
        ]
    ]
    [ box
        [ classes [ pure "text-slate-900 p-3 flex justify-center shadow-inner" ]
        ]
        [ logo ]
    , box [ classes [ pure "h-1 w-screen mb-10 bg-white shadow-md" ] ] []
    , box
        [ classes
            [ pure
                "flex-grow text-xl text-white flex flex-col items-center font-mono gap-20"
            ]
        ]
        [ text jellyIs
        , box
            [ classes
                [ ifM isComplete (pure "scale-100") (pure "scale-0")
                , pure "transition-transform"
                ]
            ]
            [ counter ]
        ]

    ]
