module Components.PopIn where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Jelly.Data.Jelly (alone, newJelly)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component)
import Utils (box)

popIn :: forall r. Array (Component r) -> Component r
popIn children = do
  isMounted /\ modifyIsMounted <- newJelly false

  _ <- liftEffect $ setTimeout 10 $ alone do
    modifyIsMounted \_ -> true

  box
    [ classes
        [ ifM isMounted (pure "scale-100 opacity-100")
            (pure "scale-50 opacity-0")
        , pure "transition-all"
        ]
    ]
    children
