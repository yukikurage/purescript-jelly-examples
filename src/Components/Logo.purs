module Components.Logo where

import Prelude

import Contexts (Contexts)
import Jelly.Data.Component (Component, text)
import Jelly.Hooks.Ch (ch)
import Utils (box, classes)

logo :: Component Contexts
logo = box do
  classes
    [ pure
        "relative text-5xl z-20 font-AlfaSlabOne font-extrabold transition-colors drop-shadow"
    ]

  ch $ text $ pure "JELLY"
