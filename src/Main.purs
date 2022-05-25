module Main where

import Prelude

import Effect (Effect)
import Jelly.Data.Props (classes)
import Jelly.HTML (el, text)
import Jelly.RunComponent (runComponent)

main :: Effect Unit
main = runComponent unit $
  el "div" [ classes [ pure "flex justify-center h-full w-full" ] ]
    [ text $ pure "Hello, Jelly" ]
