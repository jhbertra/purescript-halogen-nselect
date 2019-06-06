module Example.Extend.Autocomplete where

import Example.Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Monoid as Monoid
import Data.String as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import NSelect.Component as SC

type Message = Void

type Query = SC.Query

type Action
  = SC.Action Void

type ExtraStateRow =
  ( value :: String
  , filteredItems :: Array String
  )

type State = SC.State String ExtraStateRow

type HTML = H.ComponentHTML Action () Aff

items :: Array String
items =
  [ "purescript-css-validate"
  , "purescript-halogen-color-picker"
  , "purescript-halogen-day-picker"
  , "purescript-halogen-nselect"
  , "purescript-halogen-storybook"
  , "purescript-halogen-transition"
  , "purescript-jest"
  , "purescript-svg-parser"
  , "purescript-svgo"
  , "svgen"
  ]

initialState :: State
initialState =
  { value: ""
  , select: SC.initialState
  , items
  , filteredItems: items
  }

render :: State -> HTML
render state =
  HH.div
  ( SC.setRootProps []
  ) $ join
  [ pure $ HH.input
    ( SC.setInputProps
      [ style "width: 20rem"
      , HP.value state.value
      ]
    )
  , guard state.select.isOpen $> HH.div
    [ class_ "Dropdown"
    ]
    [ HH.ul
      ( SC.setMenuProps
        [ class_ "overflow-y-auto"
        , style "max-height: 10rem;"
        ]
      ) $ state.filteredItems # Array.mapWithIndex \index item ->
      HH.li
      ( SC.setItemProps index
        [ class_ $ "py-1 px-3 cursor-pointer" <>
            Monoid.guard (index == state.select.highlightedIndex) " bg-blue-300"
        ]
      )
      [ HH.text item ]
    ]
  ]

component :: H.Component HH.HTML Query Unit Message Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: SC.mkEval $ SC.defaultEval
      { handleMessage = handleMessage
      }
  }

type DSL = H.HalogenM State Action () Message Aff

handleMessage :: SC.Message -> DSL Unit
handleMessage = case _ of
  SC.Selected index -> do
    state <- H.get
    for_ (Array.index state.filteredItems index) \item ->
      H.modify_ $ _ { value = item, select { isOpen = false } }
  SC.InputValueChanged value -> do
    H.modify_ $ \state -> state
      { value = value
      , filteredItems = Array.filter (\s -> String.contains (String.Pattern value) s) state.items
      }
  SC.VisibilityChanged _ -> pure unit
