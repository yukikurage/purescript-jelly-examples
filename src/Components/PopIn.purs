module Components.PopIn where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Jelly.Data.Jelly (alone)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component)
import Jelly.Hooks.UseState (useState)
import Utils (box)

popIn :: forall r. Component r -> Component r
popIn child = do
  isMounted /\ modifyIsMounted <- useState false

  _ <- liftEffect $ setTimeout 100 $ alone do
    modifyIsMounted \_ -> true

  box
    [ classes
        [ pure "transition-all"
        , ifM isMounted (pure "scale-100 opacity-100")
            (pure "scale-50 opacity-0")
        ]
    ]
    [ child ]
