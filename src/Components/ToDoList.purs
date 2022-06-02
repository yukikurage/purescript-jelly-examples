module Components.ToDoList where

import Prelude

import Components.Icon (icon)
import Contexts (Contexts)
import Contexts.ColorMode (mergeColorScheme, useColorScheme)
import Data.Array (filter)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Hooks.UsePopIn (usePopIn)
import Hooks.UseTypingString (useTypingString)
import Jelly.Data.Jelly (Jelly, modify, new, read)
import Jelly.Data.Props (classes, on)
import Jelly.HTML (Component, el, elFor, elWhen, text)
import Utils (button)

type TodoListItem =
  { id :: String
  , text :: String
  , completed :: Boolean
  }

initItems :: Array TodoListItem
initItems =
  [ { id: "0"
    , text: "Learn PureScript"
    , completed: true
    }
  , { id: "1"
    , text: "Learn Jelly"
    , completed: false
    }
  , { id: "2"
    , text: "Create Web Apps"
    , completed: false
    }
  ]

getItemKey :: TodoListItem -> String
getItemKey = _.id

todoListItemComponent
  :: { todoListItem :: Jelly TodoListItem
     , deleteTodoListItem :: Jelly Unit
     , setCompleted :: Boolean -> Jelly Unit
     }
  -> Component Contexts
todoListItemComponent { todoListItem, deleteTodoListItem, setCompleted } = do
  popIn <- usePopIn
  colorScheme <- useColorScheme

  let
    textJelly = todoListItem <#> _.text
    completedJelly = todoListItem <#> _.completed

  el "div"
    [ classes
        [ pure "flex flex-row items-center gap-1 transition-transform", popIn ]
    ]
    do
      button
        [ classes
            [ pure
                "rounded-md w-8 h-8 flex flex-row justify-center items-center hover:scale-110 origin-center transition-all"
            , do
                completed <- completedJelly
                if completed then colorScheme <#> mergeColorScheme >>>
                  _.highlight
                else colorScheme <#> mergeColorScheme >>> _.reverse
            ]
        , on "click" $ \_ -> do
            completed <- completedJelly
            setCompleted $ not completed
        ]
        do
          elWhen completedJelly $ icon $ pure "fa-solid fa-check"

      el "div"
        [ classes
            [ pure
                "rounded-md p-3 w-80 flex flex-row justify-between items-center transition-colors"
            , colorScheme <#> mergeColorScheme >>> _.primary
            ]
        ]
        do
          txt <- useTypingString textJelly
          text $ txt
          button
            [ classes
                [ pure
                    "w-8 h-8 flex flex-row justify-center items-center hover:scale-110 origin-center transition-all"
                ]
            , on "click" $ \_ -> deleteTodoListItem
            ]
            do icon $ pure "fa-solid fa-trash-can"

todoList
  :: Component Contexts
todoList = do
  items <- new initItems
  el "div"
    [ classes [ pure "flex flex-col gap-2" ] ]
    do
      elFor (read items) (getItemKey >>> Just) \todoListItem ->
        todoListItemComponent
          { todoListItem
          , deleteTodoListItem: do
              tli <- todoListItem
              modify items \prevItems -> filter (\a -> a.id /= tli.id)
                prevItems
          , setCompleted: \completed -> do
              tli <- todoListItem
              modify items \prevItems ->
                mapFlipped prevItems \prevItem ->
                  if prevItem.id == tli.id then prevItem
                    { completed = completed }
                  else prevItem
          }
