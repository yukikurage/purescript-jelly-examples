module Main where

import Prelude

import Effect (Effect)
import Jelly.Data.Props (classes)
import Jelly.HTML (el, text)
import Jelly.RunComponent (runComponent)

main :: Effect Unit
main = runComponent unit $
  el "div" [ classes [ pure "flex justify-center items-center h-40 w-full" ] ]
    [ el "div" [ classes [ pure "text-3xl font-mono font-bold" ] ] [ text $ pure "Hello, Jelly" ] ]
