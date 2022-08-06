module Styles where

import Prelude

import Data.Tuple.Nested ((/\))
import Hooks.UseClass (useClass)
import Jelly (Hook, signal, useTimeout, writeAtom)

usePopIn :: forall r. Hook r Unit
usePopIn = do
  isMountedSig /\ isMountedAtom <- signal false

  useTimeout 0 $ writeAtom isMountedAtom $ true

  useClass $ ifM isMountedSig (pure "transition-all scale-100 opacity-100")
    (pure "transition-all scale-50 opacity-0")

hoverScale :: forall r. Hook r Unit
hoverScale = useClass $ pure "hover:scale-110"
