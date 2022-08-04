module Hooks.UseWindowHeight where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Jelly.Data.Hook (Hook)
import Jelly.Data.Signal (Signal, signal)
import Jelly.Hooks.UseUnmountSignal (useUnmountSignal)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window

useWindowHeight :: forall r. Hook r (Signal Int)
useWindowHeight = do
  w <- liftEffect $ window

  initialWindowHeight <- liftEffect $ Window.innerHeight w
  windowHeightSig /\ windowHeightMod <- signal initialWindowHeight

  listener <- liftEffect $ eventListener \_ -> do
    newHeight <- Window.innerHeight w
    windowHeightMod \_ -> newHeight

  liftEffect $ addEventListener (EventType "resize") listener false $
    Window.toEventTarget w

  useUnmountSignal do
    liftEffect $ removeEventListener (EventType "resize") listener false $
      Window.toEventTarget w

  pure windowHeightSig
