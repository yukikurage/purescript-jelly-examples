module Utils where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Jelly (Component, Hook, el)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

box :: forall r. Hook r Unit -> Component r
box = el "div"

button :: forall r. Hook r Unit -> Component r
button = el "button"

elEmpty :: forall r. Component r
elEmpty = el "div" $ pure unit

openLink :: forall m. MonadEffect m => String -> m Unit
openLink str = liftEffect $
  setHref
    str
    =<< location
    =<< window
