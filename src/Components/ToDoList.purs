module Components.ToDoList where

import Prelude

import Components.Icon (icon)
import Contexts (Contexts)
import Contexts.ColorMode (useColorScheme)
import Data.Array (filter)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Hooks.UseClass (useClass)
import Hooks.UseTypingString (useTypingString)
import Jelly.Data.Component (Component, text)
import Jelly.Data.Signal (Signal, modifyAtom_, readSignal, signal)
import Jelly.Hooks.Ch (ch, chWhen, chsFor)
import Jelly.Hooks.On (on)
import Styles (usePopIn)
import Utils (box, button)

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
     , deleteTodoListItem :: Effect Unit
     , setCompleted :: Boolean -> Effect Unit
     }
  -> Component Contexts
todoListItemComponent { todoListItem, deleteTodoListItem, setCompleted } = box
  do
    colorScheme <- useColorScheme

    let
      textSignal = _.text <$> todoListItem
      isCompletedSignal = _.completed <$> todoListItem

    usePopIn

    useClass $ pure "flex flex-row items-center gap-1 transition-transform"

    ch $ button do
      useClass $ pure
        "rounded-md w-8 h-8 flex flex-row justify-center items-center hover:scale-110 origin-center transition-all"
      useClass do
        completed <- isCompletedSignal
        cs <- colorScheme
        pure
          if completed then cs.highlight.text <> " " <>
            cs.highlight.background
          else cs.highlight.text <> " " <>
            cs.reverse.background

      on "click" $ \_ -> do
        completed <- readSignal isCompletedSignal
        setCompleted $ not completed

      chWhen isCompletedSignal $ icon $ pure "fa-solid fa-check"

    ch $ box do
      useClass $ pure
        "rounded-md p-3 w-60 md:w-80 flex flex-row justify-between items-center transition-colors"
      useClass $ (_.primary.text) <$> colorScheme

      txt <- useTypingString textSignal

      ch $ box do
        useClass $ pure "relative"
        ch $ box do
          useClass $ pure
            "content-[''] block absolute top-1/2 left-0 h-[2px] transition-all"
          useClass do
            completed <- isCompletedSignal
            pure $
              if completed then "opacity-100 w-full"
              else "opacity-0 w-3/4"
          useClass $ (_.reverse.background) <$> colorScheme

        ch $ box do
          useClass $ pure "p-2 transition-colors"
          useClass $ do
            completed <- isCompletedSignal
            cs <- colorScheme
            pure $
              if completed then cs.disabled.text
              else ""
          ch $ text $ txt

      ch $ button do
        useClass $ pure
          "w-8 h-8 flex flex-row justify-center items-center hover:scale-110 origin-center transition-all"

        on "click" $ \_ -> deleteTodoListItem

        ch $ icon $ pure "fa-solid fa-solid fa-xmark"

todoList
  :: Component Contexts
todoList = box do
  itemsSig /\ itemsAtom <- signal initItems

  useClass $ pure "flex flex-col gap-2"

  chsFor itemsSig (getItemKey >>> Just) \todoListItemSig ->
    todoListItemComponent
      { todoListItem: todoListItemSig
      , deleteTodoListItem: do
          tli <- readSignal todoListItemSig
          modifyAtom_ itemsAtom $ \prevItems -> filter (\a -> a.id /= tli.id)
            prevItems
      , setCompleted: \completed -> do
          tli <- readSignal todoListItemSig
          liftEffect $ modifyAtom_ itemsAtom \prevItems ->
            mapFlipped prevItems \prevItem ->
              if prevItem.id == tli.id then prevItem
                { completed = completed }
              else prevItem
      }
