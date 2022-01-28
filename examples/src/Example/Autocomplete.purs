module Example.Autocomplete where

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
import NSelect as Select
import Type.Proxy (Proxy(..))

data Action = HandleDropdown (Select.Message Action)

type State =
  { value :: String
  , items :: Array String
  , filteredItems :: Array String
  }

type Slots =
  ( dropdown :: Select.Slot Action Unit
  )

_dropdown = Proxy :: Proxy "dropdown"

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

renderSelect :: State -> Select.State -> Select.HTML Action () Aff
renderSelect state st =
  HH.div
    ( Select.setRootProps [ class_ "inline-block" ]
    ) $ join
    [ pure $ HH.input
        ( Select.setInputProps
            [ style "width: 20rem"
            , HP.value state.value
            , HP.placeholder "Autocomplete input"
            ]
        )
    , guard st.isOpen $> HH.div
        [ class_ "Dropdown"
        ]
        [ HH.div
            ( Select.setMenuProps
                [ class_ "overflow-y-auto"
                , style "max-height: 10rem;"
                ]
            ) $ state.filteredItems # Array.mapWithIndex \index item ->
            HH.div
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
  HH.slot _dropdown unit Select.component
    { render: renderSelect state
    , itemCount: Array.length state.filteredItems
    }
    HandleDropdown

component :: forall q. H.Component q Unit Void Aff
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
    void $ H.tell _dropdown unit Select.Close
  Select.InputValueChanged value -> do
    H.modify_ $ \state -> state
      { value = value
      , filteredItems = Array.filter (\s -> String.contains (String.Pattern value) s) state.items
      }
  Select.Emit q -> handleAction q
  _ -> pure unit
