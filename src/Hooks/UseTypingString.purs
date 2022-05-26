module Hooks.UseTypingString where

import Prelude

import Data.String (length, take)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Jelly.Data.Hooks (Hooks)
import Jelly.Data.Jelly (Jelly, alone)
import Jelly.Hooks.UseState (useState)
import Jelly.Hooks.UseUnmountJelly (useUnmountJelly)

useTypingString
  :: forall r. String -> Hooks r (Jelly String /\ Jelly Boolean)
useTypingString str = do
  state /\ modifyState <- useState "■"
  complete /\ modifyComplete <- useState false

  id <- liftEffect $ setInterval 50 $ alone do
    s <- state
    ifM complete
      do
        modifyState $ const $ str
      do
        modifyState $ const $ take (length s) str <> "■"
        when (length s == length str) $ modifyComplete $ const true

  useUnmountJelly $ liftEffect $ clearInterval id

  pure $ state /\ complete
