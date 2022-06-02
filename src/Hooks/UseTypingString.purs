module Hooks.UseTypingString where

import Prelude

import Data.String (length, take)
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Jelly.Data.Hooks (Hooks)
import Jelly.Data.Jelly (Jelly, alone, read, set)
import Jelly.Hooks.UseState (useState)
import Jelly.Hooks.UseUnmountJelly (useUnmountJelly)

useTypingString
  :: forall r. Jelly String -> Hooks r (Jelly String)
useTypingString strJelly = do
  stateRef <- useState ""

  id <- liftEffect $ setInterval 30 $ alone do
    targetStr <- strJelly
    stateStr <- read stateRef
    when (targetStr /= stateStr)
      if take (length stateStr) targetStr == stateStr then
        set stateRef $ take (length stateStr + 1) targetStr
      else
        set stateRef $ take (length stateStr - 1) stateStr

  useUnmountJelly $ liftEffect $ clearInterval id

  pure $ read stateRef
