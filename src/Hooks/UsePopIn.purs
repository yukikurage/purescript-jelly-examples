module Hooks.UsePopIn where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Timer (clearTimeout, setTimeout)
import Jelly.Data.Hook (Hook)
import Jelly.Data.Signal (Signal, signal)
import Jelly.Hooks.UseUnmountSignal (useUnmountSignal)

usePopIn :: forall r. Hook r (Signal String)
usePopIn = do
  isMountedSig /\ isMountedMod <- signal false

  id <- liftEffect $ setTimeout 10 $ isMountedMod $ const true

  useUnmountSignal $ liftEffect $ clearTimeout id

  pure $ ifM isMountedSig (pure "transition-all scale-100 opacity-100")
    (pure "transition-all scale-50 opacity-0")
