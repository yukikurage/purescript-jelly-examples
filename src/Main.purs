module Main where

import Prelude

import Components.Counter (counter)
import Components.Logo (logo)
import Components.PopIn (popIn)
import Contexts (Contexts)
import Contexts.ColorMode (ColorMode(..), useColorMode)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (clearTimeout, setTimeout)
import Hooks.UseTypingString (useTypingString)
import Jelly.Data.Jelly (alone, newJelly)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component, text, whenEl)
import Jelly.Hooks.UseState (useState)
import Jelly.Hooks.UseUnmountJelly (useUnmountJelly)
import Jelly.RunComponent (runComponent)
import Utils (box)

main :: Effect Unit
main = do
  colorMode /\ modifyColorMode <- newJelly Dark
  runComponent { colorMode: colorMode /\ (\x -> modifyColorMode (const x)) }
    root

root :: Component Contexts
root = do
  jellyIs /\ _ <- useTypingString
    "Jelly is a easy way to create interactive web apps."

  colorMode /\ _ <- useColorMode

  isDisplayExamples /\ modifyIsDisplayExamples <- useState false

  id <- liftEffect $ setTimeout 400 $ alone $ modifyIsDisplayExamples
    (const true)

  useUnmountJelly $ liftEffect $ clearTimeout id

  box
    [ classes
        [ pure
            "h-screen w-screen relative text-white bg-slate-900 text-lg overflow-hidden flex flex-col items-center font-Inconsolata"
        , do
            cm <- colorMode
            pure case cm of
              White -> ""
              Dark -> "dark"
        ]
    ]
    [ box
        [ classes [ pure "text-slate-900 p-3 flex justify-center shadow-inner" ]
        ]
        [ logo ]
    , box [ classes [ pure "h-1 w-screen mb-10 bg-white" ] ] []
    , text jellyIs
    , whenEl isDisplayExamples $ box
        [ classes
            [ pure
                "flex-grow flex flex-col items-center justify-center gap-10"
            ]
        ]
        [ counter ]
    , whenEl isDisplayExamples $ box [ classes [ pure "m-10" ] ]
        [ popIn
            [ text =<< fst <$> useTypingString "The Button"
            ]
        ]
    ]
