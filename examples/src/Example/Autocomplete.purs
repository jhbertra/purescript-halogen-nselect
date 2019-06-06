module Example.Autocomplete where

import Example.Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import NSelect as Select

type Query = Const Void

data Action
  = HandleDropdown (Select.Message Action)

type State =
  { value :: String
  , items :: Array String
  , filteredItems :: Array String
  }

type Slots =
  ( dropdown :: Select.Slot Action Unit
  )

_dropdown = SProxy :: SProxy "dropdown"

type HTML = H.ComponentHTML Action Slots Aff

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
  , items
  , filteredItems: items
  }

renderSelect :: State -> Select.State -> Select.HTML String Action () Aff
renderSelect state st =
  HH.div
  ( Select.setRootProps []
  ) $ join
  [ pure $ HH.input
    ( Select.setInputProps
      [ style "width: 20rem"
      , HP.value state.value
      ]
    )
  , guard st.isOpen $> HH.div
    [ class_ "Dropdown"
    ]
    [ HH.ul
      ( Select.setMenuProps
        [ class_ "overflow-y-auto"
        , style "max-height: 10rem;"
        ]
      ) $ state.filteredItems # Array.mapWithIndex \index item ->
      HH.li
      ( Select.setItemProps index
        [ class_ $ "py-1 px-3 cursor-pointer" <>
            Monoid.guard (index == st.highlightedIndex) " bg-blue-300"
        ]
      )
      [ HH.text item ]
    ]
  ]

render :: State -> HTML
render state =
  HH.div_
  [ HH.p
    [ class_ "mb-3"]
    [ HH.text "Use ArrowUp/ArrowDown to change selection, Enter to confirm."]
  , HH.slot _dropdown unit Select.component
    { render: renderSelect state
    , items: state.filteredItems
    } $ Just <<< HandleDropdown
  ]

component :: H.Component HH.HTML Query Unit Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction (HandleDropdown msg) = case msg of
  Select.Selected index -> do
    state <- H.get
    for_ (Array.index state.filteredItems index) \item ->
      H.modify_ $ _ { value = item }
    void $ H.query _dropdown unit $ H.tell Select.Close
  Select.InputValueChanged value -> do
    H.modify_ $ \state -> state
      { value = value
      , filteredItems = Array.filter (\s -> String.contains (String.Pattern value) s) state.items
      }
  Select.VisibilityChanged _ -> pure unit
  Select.Emit q -> handleAction q
