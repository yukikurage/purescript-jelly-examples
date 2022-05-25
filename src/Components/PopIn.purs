module Components.PopIn where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Timer (setTimeout)
import Jelly.Data.Jelly (alone, newJelly)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component)
import Utils (box)

popIn :: Array (Component Unit) -> Component Unit
popIn children = do
  isMounted /\ modifyIsMounted <- newJelly false

  _ <- liftEffect $ setTimeout 10 $ alone unit do
    modifyIsMounted \_ -> true

  box
    [ classes
        [ ifM isMounted (pure "scale-100 opacity-100")
            (pure "scale-50 opacity-0")
        , pure "transition-all"
        ]
    ]
    children
