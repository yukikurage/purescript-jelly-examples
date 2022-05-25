module Utils where

import Jelly.Data.Props (Prop)
import Jelly.HTML (Component, el)

box :: forall r. Array Prop -> Array (Component r) -> Component r
box = el "div"

button :: forall r. Array Prop -> Array (Component r) -> Component r
button = el "button"
