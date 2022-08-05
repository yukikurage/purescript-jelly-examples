module Components.Logo where

import Prelude

import Contexts (Contexts)
import Hooks.UseClass (useClass)
import Jelly.Data.Component (Component, text)
import Jelly.Hooks.Ch (ch)
import Utils (box)

logo :: Component Contexts
logo = box do
  useClass $ pure
    "relative text-5xl z-20 font-AlfaSlabOne font-extrabold transition-colors drop-shadow"

  ch $ text $ pure "JELLY"
