module Contexts.ColorMode where

import Prelude

import Data.Tuple.Nested (type (/\))
import Jelly.Data.Hooks (Hooks)
import Jelly.Data.Jelly (Jelly)
import Jelly.Hooks.UseContext (useContext)

data ColorMode = White | Dark

derive instance Eq ColorMode

type ColorModeContext r =
  (colorMode :: Jelly ColorMode /\ (ColorMode -> Jelly Unit) | r)

useColorMode
  :: forall r
   . Hooks (Record (ColorModeContext r))
       (Jelly ColorMode /\ (ColorMode -> Jelly Unit))
useColorMode = do
  { colorMode } <- useContext
  pure colorMode
