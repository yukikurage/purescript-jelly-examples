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
import Hooks.UseClass (useClass)
import Hooks.UsePopIn (usePopIn)
import Hooks.UseTypingString (useTypingString)
import Hooks.UseWindowHeight (useWindowHeight)
import Jelly.Data.Component (Component, text)
import Jelly.Data.Signal (modifyAtom_, signal, writeAtom)
import Jelly.Hooks.Ch (ch, chWhen)
import Jelly.Hooks.On (on)
import Jelly.Hooks.Prop ((:+))
import Jelly.Hooks.UseTimeout (useTimeout)
import Jelly.LaunchApp (launchApp)
import Utils (box, button, openLink)

main :: Effect Unit
main = do
  colorMode <- provideColorMode
  launchApp root { colorMode }

exampleTotalNum :: Int
exampleTotalNum = 2

root :: Component Contexts
root = box do
  jellyIs <- useTypingString $ pure
    "Reactivity-based Web Framework"

  colorModeSig /\ colorModeMod <- useColorMode
  colorSchemeSig <- useColorScheme

  isDisplayExamplesSig /\ isDisplayExamplesAtom <- signal false

  useTimeout 200 $ writeAtom isDisplayExamplesAtom true

  exampleNumSig /\ exampleNumAtom <- signal 0

  windowHeight <- useWindowHeight
  "style" :+ do
    wh <- windowHeight
    pure $ "height:" <> show wh <> "px;"

  useClass $ pure
    "w-screen relative text-xl overflow-hidden flex flex-col items-center font-Inconsolata transition-colors"
  useClass $ (_.primary.text) <$> colorSchemeSig
  useClass $ (_.primary.background) <$> colorSchemeSig

  ch $ box do
    useClass $ pure "py-6 px-8 flex justify-between w-screen relative"
    ch $ box $ useClass $ pure "hidden md:block w-12"
    ch $ box do
      useClass $ pure
        "absolute w-60 md:w-56 mt-7 h-5 z-10 rounded-t-md -left-6 md:left-1/2 md:transform md:-translate-x-1/2 transition-colors"
      useClass $ (_.highlight.background) <$> colorSchemeSig
      useClass $ (_.primary.text) <$> colorSchemeSig
    ch $ logo
    ch $ button do
      useClass $ pure
        "w-12 rounded-full hover:scale-110 transition-all flex justify-center items-center"

      on "click" \_ -> do
        liftEffect $ modifyAtom_ colorModeMod \cm -> case cm of
          Light -> Dark
          Dark -> Light

      ch $ icon do
        cm <- colorModeSig
        case cm of
          Light -> pure "fa-solid fa-sun fa-lg"
          Dark -> pure "fa-solid fa-moon fa-lg"

  ch $ box do
    useClass $ pure "py-6"
    ch $ text jellyIs

  chWhen isDisplayExamplesSig $ box do
    useClass $ pure
      "flex flex-grow flex-row items-center justify-center md:justify-between w-full"

    ch $ button do
      usePopIn
      useClass $ pure
        "hidden md:block h-full w-32 hover:-translate-x-1 transition-transform"

      on "click" \_ -> liftEffect
        $ modifyAtom_ exampleNumAtom
        $ \n -> (n - 1) `mod` exampleTotalNum

      ch $ icon $ pure "fa-xl fa-solid fa-chevron-left"

    chWhen ((_ == 0) <$> exampleNumSig) $ counter
    chWhen ((_ == 1) <$> exampleNumSig) $ todoList

    ch $ button do
      usePopIn
      useClass $ pure
        "hidden md:block h-full w-32 hover:translate-x-1 transition-transform"

      on "click" \_ -> liftEffect
        $ modifyAtom_ exampleNumAtom
        $ \n -> (n + 1) `mod` exampleTotalNum
      ch $ icon $ pure "fa-xl fa-solid fa-chevron-right"

  chWhen isDisplayExamplesSig $ box do
    useClass $ pure
      "flex md:hidden flex-row items-center justify-start w-full gap-2 px-8"

    ch $ button do
      usePopIn
      useClass $ pure
        "md:hidden h-16 w-12 hover:-translate-x-1 transition-transform"

      on "click" \_ -> liftEffect
        $ modifyAtom_ exampleNumAtom
        $ \n -> (n - 1) `mod` exampleTotalNum

      ch $ icon $ pure "fa-lg fa-solid fa-chevron-left"

    ch $ button do
      usePopIn
      useClass $ pure
        "md:hidden h-16 w-12 hover:translate-x-1 transition-transform"

      on "click" \_ -> liftEffect
        $ modifyAtom_ exampleNumAtom
        $ \n -> (n + 1) `mod` exampleTotalNum
      ch $ icon $ pure "fa-lg fa-solid fa-chevron-right"

  chWhen isDisplayExamplesSig $ box do
    useClass $ pure "flex flex-row w-full justify-start items-center py-6 px-8"

    ch $ button do
      useClass $ pure
        "w-12 h-12 px-2 flex justify-center items-center"
      usePopIn

      on "click" \_ -> openLink
        "https://github.com/yukikurage/purescript-jelly-examples"

      ch $ icon $ pure
        "fa-xl fa-brands fa-github flex justify-center items-center hover:scale-110 transition-all "
    ch $ box do
      useClass $ pure
        "flex-grow flex flex-row items-center justify-center w-full"

      exampleName <- useTypingString do
        en <- exampleNumSig
        pure case en of
          0 -> "Counter"
          1 -> "ToDo List"
          _ -> "Unknown"
      ch $ text exampleName

    ch $ box $ useClass $ pure "w-12"
