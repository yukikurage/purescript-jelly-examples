module Hooks.UseWindowHeight where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Jelly (Hook, Signal, signal, useEventListener, writeAtom)
import Web.HTML (window)
import Web.HTML.Window as Window

useWindowHeight :: forall r. Hook r (Signal Int)
useWindowHeight = do
  w <- liftEffect $ window

  initialWindowHeight <- liftEffect $ Window.innerHeight w

  windowHeightSig /\ windowHeightAtom <- signal initialWindowHeight

  let
    listener = \_ -> do
      newHeight <- Window.innerHeight w
      writeAtom windowHeightAtom newHeight

  useEventListener "resize" listener $
    Window.toEventTarget w

  pure windowHeightSig
