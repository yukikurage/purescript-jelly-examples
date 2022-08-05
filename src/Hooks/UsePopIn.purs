module Hooks.UsePopIn where

import Prelude

import Data.Tuple.Nested ((/\))
import Hooks.UseClass (useClass)
import Jelly.Data.Hook (Hook)
import Jelly.Data.Signal (signal, writeAtom)
import Jelly.Hooks.UseTimeout (useTimeout)

usePopIn :: forall r. Hook r Unit
usePopIn = do
  isMountedSig /\ isMountedAtom <- signal false

  useTimeout 10 $ writeAtom isMountedAtom $ true

  useClass $ ifM isMountedSig (pure "transition-all scale-100 opacity-100")
    (pure "transition-all scale-50 opacity-0")
