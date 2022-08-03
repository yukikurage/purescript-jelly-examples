module Components.Counter where

import Prelude

import Contexts (Contexts)
import Contexts.ColorMode (useColorScheme)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Hooks.UsePopIn (usePopIn)
import Jelly.Data.Component (Component, text)
import Jelly.Data.Signal (signal)
import Jelly.Hooks.Ch (ch)
import Jelly.Hooks.On (on)
import Utils (box, button, classes, elEmpty)

counter :: Component Contexts
counter = box do
  colorSchemeSig <- useColorScheme

  countSig /\ countMod <- signal 0

  popIn <- usePopIn
  classes
    [ pure "h-24 w-24 flex flex-col justify-center items-center relative"
    , popIn
    ]

  ch $ box do
    classes
      [ pure
          "h-24 w-24 -rotate-0 hover:rotate-0 transition-all absolute origin-center rounded-md"
      , (_.highlight.text) <$> colorSchemeSig
      , (_.highlight.background) <$> colorSchemeSig
      ]
    ch $ elEmpty
    ch $ button do
      classes
        [ pure
            "h-24 w-24 hover:rotate-[20deg] shadow transition-all absolute origin-center rounded-md"
        , (_.reverse.text) <$> colorSchemeSig
        , (_.reverse.background) <$> colorSchemeSig
        ]
      on "click" \_ -> liftEffect $ countMod (_ + 1)
  ch $ box do
    classes
      [ pure
          "flex justify-center items-center text-3xl z-20 relative pointer-events-none transition-colors"
      , (_.reverse.text) <$> colorSchemeSig
      ]

    ch $ text $ show <$> countSig
