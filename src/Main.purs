module Main where

import Prelude

import Components.Counter (counter)
import Components.Logo (logo)
import Components.PopIn (popIn)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Jellies.TypingEffectJelly (typingEffectJelly)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component, text, whenEl)
import Jelly.RunComponent (runComponent)
import Utils (box)

main :: Effect Unit
main = do
  runComponent unit root

root :: Component Unit
root = do
  jellyIs /\ isComplete <- typingEffectJelly
    "Jelly is a easy way to create interactive web apps."

  box
    [ classes
        [ pure
            "h-screen w-screen relative text-white bg-slate-900 text-lg overflow-hidden flex flex-col items-center font-Inconsolata"
        ]
    ]
    [ box
        [ classes [ pure "text-slate-900 p-3 flex justify-center shadow-inner" ]
        ]
        [ logo ]
    , box [ classes [ pure "h-1 w-screen mb-10 bg-white shadow-md" ] ] []
    , text jellyIs
    , whenEl isComplete $ box
        [ classes
            [ pure
                "flex-grow flex flex-col items-center justify-center gap-10"
            ]
        ]
        [ counter ]
    , whenEl isComplete $ box [ classes [ pure "m-10" ] ]
        [ popIn
            [ do
                title /\ _ <- typingEffectJelly "The Button"
                text title
            ]
        ]
    ]
