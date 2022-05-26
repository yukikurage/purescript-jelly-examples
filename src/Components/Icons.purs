module Components.Icons where

import Prelude

import Jelly.Data.Props (Prop, (@=))
import Jelly.HTML (Component, elNS)

{- Icons from [HeroIcons](https://heroicons.com/) -}

svgEl
  :: forall r. String -> Array Prop -> Array (Component r) -> Component r
svgEl = elNS "http://www.w3.org/2000/svg"

chevronLeft :: forall r. Component r
chevronLeft = svgEl "svg"
  [ "fill" @= pure "none"
  , "viewBox" @= pure "0 0 24 24"
  , "stroke" @= pure "currentColor"
  , "stroke-width" @= pure "2"
  ]
  [ svgEl "path"
      [ "stroke-linecap" @= pure "round"
      , "stroke-linejoin" @= pure "round"
      , "d" @= pure "M15 19l-7-7 7-7"
      ]
      []
  ]

chevronRight :: forall r. Component r
chevronRight = svgEl "svg"
  [ "fill" @= pure "none"
  , "viewBox" @= pure "0 0 24 24"
  , "stroke" @= pure "currentColor"
  , "stroke-width" @= pure "2"
  ]
  [ svgEl "path"
      [ "stroke-linecap" @= pure "round"
      , "stroke-linejoin" @= pure "round"
      , "d" @= pure "M9 5l7 7-7 7"
      ]
      []
  ]
