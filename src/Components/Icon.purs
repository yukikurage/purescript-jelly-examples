module Components.Icon where

import Prelude

import Jelly.Data.Component (Component, el)
import Jelly.Data.Signal (Signal)
import Utils (classes)

icon :: forall r. Signal String -> Component r
icon fa = el "i" $ classes [ fa ]
