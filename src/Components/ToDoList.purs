module Components.ToDoList where

import Prelude

import Components.Icon (icon)
import Contexts (Contexts)
import Contexts.ColorMode (useColorScheme)
import Data.Array (filter)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Hooks.UsePopIn (usePopIn)
import Hooks.UseTypingString (useTypingString)
import Jelly.Data.Component (Component, text)
import Jelly.Data.Signal (Signal, signal)
import Jelly.Hooks.Ch (ch, chWhen, chsFor)
import Jelly.Hooks.On (on)
import Utils (box, button, classes)

type TodoListItem =
  { id :: String
  , text :: String
  , completed :: Boolean
  }

initItems :: Array TodoListItem
initItems =
  [ { id: "0"
    , text: "Jelly"
    , completed: true
    }
  , { id: "1"
    , text: "Example"
    , completed: false
    }
  , { id: "2"
    , text: "PureScript"
    , completed: false
    }
  ]

getItemKey :: TodoListItem -> String
getItemKey = _.id

todoListItemComponent
  :: { todoListItem :: Signal TodoListItem
     , deleteTodoListItem :: Signal Unit
     , setCompleted :: Boolean -> Signal Unit
     }
  -> Component Contexts
todoListItemComponent { todoListItem, deleteTodoListItem, setCompleted } = box
  do
    popIn <- usePopIn
    colorScheme <- useColorScheme

    let
      textSignal = _.text <$> todoListItem
      isCompletedSignal = _.completed <$> todoListItem
    classes

      [ pure "flex flex-row items-center gap-1 transition-transform"
      , popIn
      ]

    ch $ button do
      classes
        [ pure
            "rounded-md w-8 h-8 flex flex-row justify-center items-center hover:scale-110 origin-center transition-all"
        , do
            completed <- isCompletedSignal
            cs <- colorScheme
            pure
              if completed then cs.highlight.text <> " " <>
                cs.highlight.background
              else cs.highlight.text <> " " <>
                cs.reverse.background
        ]
      on "click" $ \_ -> do
        completed <- isCompletedSignal
        setCompleted $ not completed

      chWhen isCompletedSignal $ icon $ pure "fa-solid fa-check"

    ch $ box do
      classes
        [ pure
            "rounded-md p-3 w-80 flex flex-row justify-between items-center transition-colors"
        , (_.primary.text) <$> colorScheme
        ]
      txt <- useTypingString textSignal

      ch $ box do
        classes [ pure "relative" ]
        ch $ box do
          classes
            [ pure
                "content-[''] block absolute top-1/2 left-0 h-[2px] transition-all"
            , do
                completed <- isCompletedSignal
                pure $
                  if completed then "opacity-100 w-full"
                  else "opacity-0 w-3/4"
            , (_.reverse.background) <$> colorScheme
            ]

        ch $ box do
          classes
            [ pure "p-2 transition-colors"
            , do
                completed <- isCompletedSignal
                cs <- colorScheme
                pure $
                  if completed then cs.disabled.text
                  else ""
            ]
          ch $ text $ txt
      ch $ button do
        classes
          [ pure
              "w-8 h-8 flex flex-row justify-center items-center hover:scale-110 origin-center transition-all"
          ]
        on "click" $ \_ -> deleteTodoListItem

        ch $ icon $ pure "fa-solid fa-solid fa-xmark"

todoList
  :: Component Contexts
todoList = box do
  itemsSig /\ itemsMod <- signal initItems

  classes [ pure "flex flex-col gap-2" ]

  chsFor itemsSig (getItemKey >>> Just) \todoListItemSig ->
    todoListItemComponent
      { todoListItem: todoListItemSig
      , deleteTodoListItem: do
          tli <- todoListItemSig
          liftEffect $ itemsMod \prevItems -> filter (\a -> a.id /= tli.id)
            prevItems
      , setCompleted: \completed -> do
          tli <- todoListItemSig
          liftEffect $ itemsMod \prevItems ->
            mapFlipped prevItems \prevItem ->
              if prevItem.id == tli.id then prevItem
                { completed = completed }
              else prevItem
      }
