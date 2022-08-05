module Components.Counter where

import Prelude

import Contexts (Contexts)
import Contexts.ColorMode (useColorScheme)
import Data.Tuple.Nested ((/\))
import Hooks.UseClass (useClass)
import Hooks.UsePopIn (usePopIn)
import Jelly.Data.Component (Component, text)
import Jelly.Data.Signal (modifyAtom_, signal)
import Jelly.Hooks.Ch (ch)
import Jelly.Hooks.On (on)
import Utils (box, button, elEmpty)

counter :: Component Contexts
counter = box do
  colorSchemeSig <- useColorScheme

  countSig /\ countAtom <- signal 0

  usePopIn
  useClass $ pure "h-24 w-24 flex flex-col justify-center items-center relative"

  ch $ box do
    useClass $ pure
      "h-24 w-24 -rotate-0 hover:rotate-0 transition-all absolute origin-center rounded-md"
    useClass $ (_.highlight.text) <$> colorSchemeSig
    useClass $ (_.highlight.background) <$> colorSchemeSig

    ch $ elEmpty
    ch $ button do
      useClass $ pure
        "h-24 w-24 hover:rotate-[20deg] shadow transition-all absolute origin-center rounded-md"
      useClass $ (_.reverse.text) <$> colorSchemeSig
      useClass $ (_.reverse.background) <$> colorSchemeSig

      on "click" \_ -> modifyAtom_ countAtom (_ + 1)

  ch $ box do
    useClass $ pure
      "flex justify-center items-center text-3xl z-20 relative pointer-events-none transition-colors"
    useClass $ (_.reverse.text) <$> colorSchemeSig

    ch $ text $ show <$> countSig
