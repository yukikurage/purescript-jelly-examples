module Components.Icon where

import Prelude

import Hooks.UseClass (useClass)
import Jelly.Data.Component (Component, el)
import Jelly.Data.Signal (Signal)

icon :: forall r. Signal String -> Component r
icon fa = el "i" $ useClass fa
