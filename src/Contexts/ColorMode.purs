module Contexts.ColorMode where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Jelly.Data.Hook (Hook)
import Jelly.Data.Signal (Signal, signal)
import Jelly.Hooks.UseContext (useContext)

data ColorMode = Light | Dark

type ColorSchemeInternal =
  { text :: String
  , background :: String
  }

type ColorScheme =
  { primary :: ColorSchemeInternal
  , highlight :: ColorSchemeInternal
  , reverse :: ColorSchemeInternal
  }

derive instance Eq ColorMode

type ColorModeContext r =
  ( colorMode :: Signal ColorMode /\ ((ColorMode -> ColorMode) -> Effect Unit)
  | r
  )

useColorMode
  :: forall r
   . Hook (Record (ColorModeContext r))
       ( Signal ColorMode /\ ((ColorMode -> ColorMode) -> Effect Unit)
       )
useColorMode = do
  { colorMode } <- useContext
  pure colorMode

useColorScheme
  :: forall r
   . Hook (Record (ColorModeContext r))
       (Signal ColorScheme)
useColorScheme = do
  colorModeSig /\ _ <- useColorMode
  pure $ getColorScheme <$> colorModeSig

provideColorMode
  :: forall m
   . MonadEffect m
  => m (Signal ColorMode /\ ((ColorMode -> ColorMode) -> Effect Unit))
provideColorMode = signal Light

getColorScheme :: ColorMode -> ColorScheme
getColorScheme = case _ of
  Light ->
    { primary:
        { text: "text-slate-900"
        , background: "bg-white"
        }
    , highlight:
        { text: "text-slate-900"
        , background: "bg-pink-500"
        }
    , reverse:
        { text: "text-white"
        , background: "bg-slate-900"
        }
    }
  Dark ->
    { primary:
        { text: "text-white"
        , background: "bg-slate-900"
        }
    , highlight:
        { text: "text-white"
        , background: "bg-pink-600"
        }
    , reverse:
        { text: "text-slate-900"
        , background: "bg-white"
        }
    }
