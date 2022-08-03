module Main where

import Prelude

import Components.Counter (counter)
import Components.Icon (icon)
import Components.Logo (logo)
import Components.ToDoList (todoList)
import Contexts (Contexts)
import Contexts.ColorMode (ColorMode(..), provideColorMode, useColorMode, useColorScheme)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (clearTimeout, setTimeout)
import Hooks.UsePopIn (usePopIn)
import Hooks.UseTypingString (useTypingString)
import Jelly.Data.Component (Component, text)
import Jelly.Data.Signal (signal)
import Jelly.Hooks.Ch (ch, chWhen)
import Jelly.Hooks.On (on)
import Jelly.Hooks.UseUnmountSignal (useUnmountSignal)
import Jelly.LaunchApp (launchApp)
import Utils (box, button, classes, openLink)

main :: Effect Unit
main = do
  colorMode <- provideColorMode
  launchApp root { colorMode }

exampleTotalNum :: Int
exampleTotalNum = 2

root :: Component Contexts
root = box do
  jellyIs <- useTypingString $ pure
    "An easy way to create interactive web apps."

  colorModeSig /\ colorModeMod <- useColorMode
  colorSchemeSig <- useColorScheme

  isDisplayExamplesSig /\ isDisplayExamplesMod <- signal false

  id <- liftEffect $ setTimeout 200 $ isDisplayExamplesMod $ const true

  useUnmountSignal $ liftEffect $ clearTimeout id

  exampleNumSig /\ exampleNumMod <- signal 0

  classes
    [ pure
        "h-screen w-screen relative text-xl overflow-hidden flex flex-col items-center font-Inconsolata transition-colors"
    , (_.primary.text) <$> colorSchemeSig
    , (_.primary.background) <$> colorSchemeSig
    ]

  ch $ box do
    classes [ pure "py-6 px-8 flex justify-between w-screen" ]
    ch $ box $ classes [ pure "w-12" ]
    ch $ logo
    ch $ button do
      classes
        [ pure
            "w-12 rounded-full hover:scale-110 transition-all flex justify-center items-center"
        ]
      on "click" \_ -> do
        liftEffect $ colorModeMod \cm -> case cm of
          Light -> Dark
          Dark -> Light

      ch $ icon do
        cm <- colorModeSig
        case cm of
          Light -> pure "fa-solid fa-sun fa-lg"
          Dark -> pure "fa-solid fa-moon fa-lg"

  ch $ box do
    classes [ pure "py-6" ]
    ch $ text jellyIs

  chWhen isDisplayExamplesSig $ box do
    classes
      [ pure
          "flex-grow flex flex-row items-center justify-between w-full"
      ]
    popIn <- usePopIn

    ch $ button do
      classes
        [ pure
            "h-full w-32 hover:-translate-x-1 transition-transform"
        , popIn
        ]
      on "click" \_ -> liftEffect
        $ exampleNumMod
        $ \n -> (n - 1) `mod` exampleTotalNum

      ch $ icon $ pure "fa-xl fa-solid fa-chevron-left"

    chWhen ((_ == 0) <$> exampleNumSig) $ counter
    chWhen ((_ == 1) <$> exampleNumSig) $ todoList

    ch $ button do
      classes
        [ pure
            "h-full w-32 hover:translate-x-1 transition-transform"
        , popIn
        ]
      on "click" \_ -> liftEffect
        $ exampleNumMod
        $ \n -> (n + 1) `mod` exampleTotalNum
      ch $ icon $ pure "fa-xl fa-solid fa-chevron-right"

  chWhen isDisplayExamplesSig $ box do
    classes
      [ pure "flex flex-row w-full justify-start items-center py-6 px-8" ]
    popIn <- usePopIn

    ch $ button do
      classes
        [ pure
            "w-12 h-12 px-2 flex justify-center items-center"
        , popIn
        ]
      on "click" \_ -> openLink
        "https://github.com/yukikurage/purescript-jelly-examples"

      ch $ icon $ pure
        "fa-xl fa-brands fa-github flex justify-center items-center hover:scale-110 transition-all "
    ch $ box do
      classes
        [ pure
            "flex-grow flex flex-row items-center justify-center w-full"
        ]
      exampleName <- useTypingString do
        en <- exampleNumSig
        pure case en of
          0 -> "Counter"
          1 -> "ToDo List"
          _ -> "Unknown"
      ch $ text exampleName

    ch $ box $ classes [ pure "w-12" ]
