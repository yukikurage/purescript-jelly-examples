module Jellies.TypingEffectJelly where

import Prelude

import Control.Monad.Reader (ask)
import Data.String (length, take)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Jelly.Data.Jelly (Jelly, addCleaner, alone, newJelly)

typingEffectJelly
  :: forall r. String -> Jelly r (Jelly r String /\ Jelly r Boolean)
typingEffectJelly str = do
  state /\ modifyState <- newJelly "■"
  complete /\ modifyComplete <- newJelly false

  { context } <- ask

  id <- liftEffect $ setInterval 40 $ alone context do
    s <- state
    ifM complete
      do
        modifyState $ const $ str
      do
        modifyState $ const $ take (length s) str <> "■"
        when (length s == length str) $ modifyComplete $ const true

  addCleaner $ liftEffect $ clearInterval id

  pure $ state /\ complete
