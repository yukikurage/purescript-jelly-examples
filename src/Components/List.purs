module Components.List where

import Prelude

import Contexts (Contexts)
import Jelly.Data.Jelly (Jelly)
import Jelly.Data.Props (classes)
import Jelly.HTML (Component, el, text)

listItem :: Jelly String -> Component Contexts
listItem txt = do
  el "div"
    [ classes [ pure "rounded-md p-10 flex justify-center items-center" ] ]
    do
      el "div" [] $ text txt

-- list :: Component Contexts
-- list
