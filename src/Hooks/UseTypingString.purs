module Hooks.UseTypingString where

import Prelude

import Data.String (length, take)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Jelly.Data.Hook (Hook)
import Jelly.Data.Signal (Signal, detach, signal)
import Jelly.Hooks.UseUnmountSignal (useUnmountSignal)

useTypingString
  :: forall r. Signal String -> Hook r (Signal String)
useTypingString strJelly = do
  stateSig /\ stateMod <- signal ""

  id <- liftEffect $ setInterval 20 $ detach do
    targetStr <- strJelly
    stateStr <- stateSig
    liftEffect $ when (targetStr /= stateStr)
      if take (length stateStr) targetStr == stateStr then
        stateMod $ const $ take (length stateStr + 1) targetStr
      else
        stateMod $ const $ take (length stateStr - 1) stateStr

  useUnmountSignal $ liftEffect $ clearInterval id

  pure $ stateSig
