module Contexts.ColorMode where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Jelly.Data.Hooks (Hooks)
import Jelly.Data.Jelly (Jelly)
import Jelly.Hooks.UseContext (useContext)

data ColorMode = Light | Dark

type ColorSchemeInternal =
  { text :: String
  , background :: String
  }

type ColorScheme internal =
  { primary :: internal
  , highlight :: internal
  , reverse :: internal
  }

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

getColorScheme :: ColorMode -> ColorScheme ColorSchemeInternal
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

mergeColorScheme :: ColorScheme ColorSchemeInternal -> ColorScheme String
mergeColorScheme colorScheme =
  let
    merge :: ColorSchemeInternal -> String
    merge colorSchemeInternal = colorSchemeInternal.text <> " " <>
      colorSchemeInternal.background
  in
    { primary: merge colorScheme.primary
    , highlight: merge colorScheme.highlight
    , reverse: merge colorScheme.reverse
    }

useColorScheme
  :: forall r
   . Hooks (Record (ColorModeContext r))
       (Jelly (ColorScheme ColorSchemeInternal))
useColorScheme = do
  colorMode /\ _ <- useColorMode
  pure $ getColorScheme <$> colorMode
