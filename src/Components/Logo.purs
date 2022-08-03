module Components.Logo where

import Prelude

import Contexts (Contexts)
import Contexts.ColorMode (useColorScheme)
import Jelly.Data.Component (Component, text)
import Jelly.Hooks.Ch (ch)
import Utils (box, classes)

logo :: Component Contexts
logo = box do
  colorSchemeSig <- useColorScheme

  classes
    [ pure
        "relative w-56 mt-6 h-5 z-10  flex flex-row justify-center items-end rounded-t-md transition-colors"
    , (_.highlight.background) <$> colorSchemeSig
    , (_.primary.text) <$> colorSchemeSig
    ]

  ch $ box do
    classes
      [ pure
          "relative text-5xl z-20 font-AlfaSlabOne font-extrabold transition-colors drop-shadow"
      ]

    ch $ text $ pure "JELLY"
