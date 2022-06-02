module Hooks.UsePopIn where

import Prelude

import Effect.Class (liftEffect)
import Effect.Timer (clearTimeout, setTimeout)
import Jelly.Data.Hooks (Hooks)
import Jelly.Data.Jelly (Jelly, alone, read, set)
import Jelly.Hooks.UseState (useState)
import Jelly.Hooks.UseUnmountJelly (useUnmountJelly)

usePopIn :: forall r. Hooks r (Jelly String)
usePopIn = do
  isMounted <- useState false

  id <- liftEffect $ setTimeout 10 $ alone do
    set isMounted true

  useUnmountJelly $ liftEffect $ clearTimeout id

  pure $ ifM (read isMounted) (pure "transition-all scale-100 opacity-100")
    (pure "transition-all scale-50 opacity-0")
