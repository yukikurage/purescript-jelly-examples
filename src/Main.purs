module Main where

import Prelude

import Components.Counter (counter)
import Components.Icon (icon)
import Components.Logo (logo)
import Components.PopIn (popIn)
import Contexts (Contexts)
import Contexts.ColorMode (ColorMode(..), mergeColorScheme, useColorMode, useColorScheme)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Timer (clearTimeout, setTimeout)
import Hooks.UseTypingString (useTypingString)
import Jelly.Data.Jelly (alone, newJelly)
import Jelly.Data.Props (classes, on)
import Jelly.HTML (Component, text, whenEl)
import Jelly.Hooks.UseState (useState)
import Jelly.Hooks.UseUnmountJelly (useUnmountJelly)
import Jelly.RunComponent (runComponent)
import Utils (box, button)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

main :: Effect Unit
main = do
  colorMode /\ modifyColorMode <- newJelly White
  runComponent { colorMode: colorMode /\ (\x -> modifyColorMode (const x)) }
    root

openLink :: forall m. MonadEffect m => String -> m Unit
openLink str = liftEffect $
  setHref
    str
    =<< location
    =<< window

root :: Component Contexts
root = do
  jellyIs /\ _ <- useTypingString
    "An easy way to create interactive web apps."

  colorMode /\ setColorMode <- useColorMode
  colorScheme <- useColorScheme

  isDisplayExamples /\ modifyIsDisplayExamples <- useState false

  id <- liftEffect $ setTimeout 400 $ alone $ modifyIsDisplayExamples
    (const true)

  useUnmountJelly $ liftEffect $ clearTimeout id

  box
    [ classes
        [ pure
            "h-screen w-screen relative text-xl overflow-hidden flex flex-col items-center font-Inconsolata transition-colors"
        , colorScheme <#> mergeColorScheme >>> _.primary
        ]
    ]
    [ box
        [ classes [ pure "py-3 px-8 flex justify-between w-screen" ]
        ]
        [ box [ classes [ pure "w-12" ] ] []
        , logo
        , button
            [ classes
                [ pure
                    "w-12 rounded-full hover:scale-110 transition-all flex justify-center items-center"
                ]
            , on "click" \_ -> do
                cm <- colorMode
                case cm of
                  White -> setColorMode Dark
                  Dark -> setColorMode White
            ]
            [ icon do
                cm <- colorMode
                case cm of
                  White -> pure "fa-solid fa-sun fa-lg"
                  Dark -> pure "fa-solid fa-moon fa-lg"
            ]
        ]
    -- , box
    --     [ classes
    --         [ pure "h-1 w-screen transition-colors"
    --         , colorScheme <#> mergeColorScheme >>> _.reverse
    --         ]
    --     ]
    --     []
    , box [ classes [ pure "py-6" ] ]
        [ text jellyIs ]
    , whenEl isDisplayExamples $ box
        [ classes
            [ pure
                "flex-grow flex flex-row items-center justify-between w-full"
            ]
        ]
        [ button
            [ classes
                [ pure
                    "h-full w-32 hover:-translate-x-1 transition-transform"
                ]
            ]
            [ popIn $ icon $ pure "fa-xl fa-solid fa-chevron-left" ]
        , counter
        , button
            [ classes
                [ pure
                    "h-full w-32 hover:translate-x-1 transition-transform"
                ]
            ]
            [ popIn $ icon $ pure "fa-xl fa-solid fa-chevron-right" ]
        ]
    , whenEl isDisplayExamples $ box
        [ classes
            [ pure "flex flex-row w-full justify-start items-center p-10" ]
        ]
        [ button
            [ classes
                [ pure
                    "w-12 h-12 px-2 flex justify-center items-center"
                ]
            , on "click" \_ -> openLink
                "https://github.com/yukikurage/purescript-jelly-examples"
            ]
            [ popIn $ icon $ pure
                "fa-xl fa-brands fa-github flex justify-center items-center hover:scale-110 transition-all "
            ]
        , box
            [ classes
                [ pure
                    "flex-grow flex flex-row items-center justify-center w-full"
                ]
            ]
            [ text =<< fst <$> useTypingString "A Button" ]
        , box [ classes [ pure "w-12" ] ] []
        ]
    ]
