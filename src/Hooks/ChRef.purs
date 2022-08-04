module Hooks.ChRef where

import Prelude

import Control.Monad.Reader (ask)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Jelly.Data.Component (Component, runComponent)
import Jelly.Data.Hook (Hook)
import Jelly.Hooks.UseUnmountSignal (useUnmountSignal)
import Web.DOM (Node)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild)

chRef :: forall r. Component r -> Hook r Node
chRef component = do
  { parentElement, context } <- ask

  node /\ unmountEffect <- liftEffect $ runComponent component context
  liftEffect $ appendChild node $ toNode parentElement

  useUnmountSignal $ liftEffect do
    unmountEffect

  pure node
