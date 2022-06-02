module Hooks.UseTypingString where

import Prelude

import Data.String (length, take)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Jelly.Data.Hooks (Hooks)
import Jelly.Data.Jelly (Jelly, alone, read, set)
import Jelly.Hooks.UseState (useState)
import Jelly.Hooks.UseUnmountJelly (useUnmountJelly)

useTypingString
  :: forall r. String -> Hooks r (Jelly String /\ Jelly Boolean)
useTypingString str = do
  stateRef <- useState "■"
  completeRef <- useState false

  id <- liftEffect $ setInterval 50 $ alone do
    s <- read stateRef
    ifM (read completeRef)
      do
        set stateRef str
      do
        set stateRef $ take (length s) str <> "■"
        when (length s == length str) $ set completeRef true

  useUnmountJelly $ liftEffect $ clearInterval id

  pure $ read stateRef /\ read completeRef
