module Hooks.UseClass where

import Prelude

import Jelly (Hook, Signal, (:+))

useClass :: forall r. Signal String -> Hook r Unit
useClass cssSig = do
  "class" :+ do
    css <- cssSig
    pure $ " " <> css
