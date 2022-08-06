module Contexts.ColorMode where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Jelly.Data.Hook (Hook)
import Jelly.Data.Signal (Atom, Signal, launch_, signal)
import Jelly.Hooks.UseContext (useContext)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

data ColorMode = Light | Dark

type ColorSchemeInternal =
  { text :: String
  , background :: String
  }

type ColorScheme =
  { primary :: ColorSchemeInternal
  , highlight :: ColorSchemeInternal
  , reverse :: ColorSchemeInternal
  , disabled :: ColorSchemeInternal
  }

derive instance Eq ColorMode
instance Show ColorMode where
  show Light = "light"
  show Dark = "dark"

type ColorModeContext r =
  ( colorMode :: Signal ColorMode /\ Atom ColorMode
  | r
  )

useColorMode
  :: forall r
   . Hook (Record (ColorModeContext r))
       (Signal ColorMode /\ Atom ColorMode)
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
  :: Effect (Signal ColorMode /\ Atom ColorMode)
provideColorMode = do
  storage <- liftEffect $ localStorage =<< window
  cmStr <- liftEffect $ getItem "colorMode" storage
  let
    savedColorMode = case cmStr of
      Just "dark" -> Dark
      _ -> Light

  colorModeSig /\ colorModAtom <- signal savedColorMode

  launch_ do
    colorMode <- colorModeSig
    liftEffect $ setItem "colorMode" (show colorMode) storage

  pure $ colorModeSig /\ colorModAtom

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
    , disabled:
        { text: "text-slate-600"
        , background: "bg-white bg-opacity-80"
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
    , disabled:
        { text: "text-slate-300"
        , background: "bg-slate-900 bg-opacity-80"
        }
    }
