module Main where

import Prelude

import Components.Counter (counter)
import Components.Icon (icon)
import Components.Logo (logo)
import Components.ToDoList (todoList)
import Contexts (Contexts)
import Contexts.ColorMode (ColorMode(..), mergeColorScheme, useColorMode, useColorScheme)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Timer (clearTimeout, setTimeout)
import Hooks.UsePopIn (usePopIn)
import Hooks.UseTypingString (useTypingString)
import Jelly.Data.Jelly (modify, new, read, set)
import Jelly.Data.Props (classes, on)
import Jelly.HTML (Component, elEmpty, elWhen, text)
import Jelly.Hooks.UseState (useState)
import Jelly.Hooks.UseUnmountJelly (useUnmountJelly)
import Jelly.RunComponent (runComponent)
import Utils (box, button)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

main :: Effect Unit
main = do
  colorMode <- new Light
  runComponent { colorMode } root

openLink :: forall m. MonadEffect m => String -> m Unit
openLink str = liftEffect $
  setHref
    str
    =<< location
    =<< window

exampleTotalNum :: Int
exampleTotalNum = 2

root :: Component Contexts
root = do
  jellyIs <- useTypingString $ pure
    "An easy way to create interactive web apps."

  colorMode <- useColorMode
  colorScheme <- useColorScheme

  isDisplayExamples <- useState false

  id <- liftEffect $ setTimeout 200 $ set isDisplayExamples true

  useUnmountJelly $ liftEffect $ clearTimeout id

  exampleNum <- useState 0

  box
    [ classes
        [ pure
            "h-screen w-screen relative text-xl overflow-hidden flex flex-col items-center font-Inconsolata transition-colors"
        , colorScheme <#> mergeColorScheme >>> _.primary
        ]
    ]
    do
      box
        [ classes [ pure "py-6 px-8 flex justify-between w-screen" ]
        ]
        do
          box [ classes [ pure "w-12" ] ] elEmpty
          logo
          button
            [ classes
                [ pure
                    "w-12 rounded-full hover:scale-110 transition-all flex justify-center items-center"
                ]
            , on "click" \_ -> do
                cm <- read colorMode
                case cm of
                  Light -> set colorMode Dark
                  Dark -> set colorMode Light
            ]
            do
              icon do
                cm <- read colorMode
                case cm of
                  Light -> pure "fa-solid fa-sun fa-lg"
                  Dark -> pure "fa-solid fa-moon fa-lg"

      box [ classes [ pure "py-6" ] ] $ text jellyIs

      elWhen (read isDisplayExamples) $ box
        [ classes
            [ pure
                "flex-grow flex flex-row items-center justify-between w-full"
            ]
        ]
        do
          popIn <- usePopIn
          button
            [ classes
                [ pure
                    "h-full w-32 hover:-translate-x-1 transition-transform"
                , popIn
                ]
            , on "click" \_ ->
                modify exampleNum $ \n -> (n - 1) `mod` exampleTotalNum
            ]
            $ icon
            $ pure "fa-xl fa-solid fa-chevron-left"

          elWhen ((_ == 0) <$> read exampleNum) $ counter
          elWhen ((_ == 1) <$> read exampleNum) $ todoList

          button
            [ classes
                [ pure
                    "h-full w-32 hover:translate-x-1 transition-transform"
                , popIn
                ]
            , on "click" \_ ->
                modify exampleNum $ \n -> (n + 1) `mod` exampleTotalNum
            ]
            $ icon
            $ pure "fa-xl fa-solid fa-chevron-right"

      elWhen (read isDisplayExamples) $ box
        [ classes
            [ pure "flex flex-row w-full justify-start items-center py-6 px-8" ]
        ]
        do
          popIn <- usePopIn
          button
            [ classes
                [ pure
                    "w-12 h-12 px-2 flex justify-center items-center"
                , popIn
                ]
            , on "click" \_ -> openLink
                "https://github.com/yukikurage/purescript-jelly-examples"
            ]
            $ icon
            $ pure
                "fa-xl fa-brands fa-github flex justify-center items-center hover:scale-110 transition-all "

          box
            [ classes
                [ pure
                    "flex-grow flex flex-row items-center justify-center w-full"
                ]
            ]
            $ text =<< useTypingString do
                en <- read exampleNum
                pure case en of
                  0 -> "Counter"
                  1 -> "ToDo List"
                  _ -> "Unknown"

          box [ classes [ pure "w-12" ] ] elEmpty
