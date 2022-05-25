module Main where

import Prelude

import Effect (Effect)
import Jelly.HTML (text)
import Jelly.RunComponent (runComponent)

main :: Effect Unit
main = runComponent unit $ text $ pure "Hello, Jelly"
