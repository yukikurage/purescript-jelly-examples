module Contexts.ColorMode where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Jelly.Data.Hooks (Hooks)
import Jelly.Data.Jelly (Jelly)
import Jelly.Hooks.UseContext (useContext)

data ColorMode = White | Dark

type ColorSchemeInternal =
  { primary :: String
  , highlight :: String
  , reverse :: String
  }

type ColorScheme =
  { text :: ColorSchemeInternal
  , background :: ColorSchemeInternal
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

getColorScheme :: ColorMode -> ColorScheme
getColorScheme = case _ of
  White ->
    { text:
        { primary: "text-slate-900"
        , highlight: "text-pink-600"
        , reverse: "text-white"
        }
    , background:
        { primary: "bg-white"
        , highlight: "bg-pink-600"
        , reverse: "bg-slate-900"
        }
    }
  Dark ->
    { text:
        { primary: "text-white"
        , highlight: "text-pink-600"
        , reverse: "text-slate-900"
        }
    , background:
        { primary: "bg-slate-900"
        , highlight: "bg-pink-600"
        , reverse: "bg-white"
        }
    }

useColorScheme
  :: forall r. Hooks (Record (ColorModeContext r)) (Jelly ColorScheme)
useColorScheme = do
  colorMode /\ _ <- useColorMode
  pure $ getColorScheme <$> colorMode
