module Hooks.UseClass where

import Prelude

import Jelly.Data.Hook (Hook)
import Jelly.Data.Signal (Signal)
import Jelly.Hooks.Prop ((:+))

useClass :: forall r. Signal String -> Hook r Unit
useClass cssSig = do
  "class" :+ do
    css <- cssSig
    pure $ " " <> css
