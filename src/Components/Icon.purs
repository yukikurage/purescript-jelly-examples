module Components.Icon where

import Jelly.Data.Jelly (Jelly)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component, el)

icon :: forall r. Jelly String -> Component r
icon fa = el "i" [ classes [ fa ] ] []
