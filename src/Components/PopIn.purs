module Components.PopIn where

import Prelude

import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Jelly.Data.Jelly (alone, read, set)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component)
import Jelly.Hooks.UseState (useState)
import Utils (box)

popIn :: forall r. Component r -> Component r
popIn child = do
  isMounted <- useState false

  _ <- liftEffect $ setTimeout 100 $ alone do
    set isMounted true

  box
    [ classes
        [ pure "transition-all"
        , ifM (read isMounted) (pure "scale-100 opacity-100")
            (pure "scale-50 opacity-0")
        ]
    ]
    child
