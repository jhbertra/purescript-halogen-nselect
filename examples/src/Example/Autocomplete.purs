module Example.Autocomplete where

import Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
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
  }

type Slots =
  ( dropdown :: Select.Slot Action Unit
  )

_dropdown = SProxy :: SProxy "dropdown"

type HTML = H.ComponentHTML Action Slots Aff

items :: Array String
items =
  [ "surprise"
  , "items"
  , "are"
  , "fixed"
  ]

initialState :: State
initialState =
  { value: ""
  , items
  }

style :: forall r i. String -> HH.IProp ("style" :: String | r) i
style = HH.attr (HH.AttrName "style")

renderSelect :: State -> Select.State -> Select.HTML Action () Aff
renderSelect state st =
  HH.div
  ( Select.setRootProps []
  ) $ join
  [ pure $ HH.input
    ( Select.setInputProps
      [ HP.value state.value
      ]
    )
  , guard st.isOpen $> HH.div_
    [ HH.ul
      [ style "list-style: none;"
      ] $ state.items # Array.mapWithIndex \index item ->
      HH.li
      ( Select.setItemProps index
        [ style $ getStyle index ]
      )
      [ HH.text item ]
    ]
  ]
  where
  getStyle index =
    if index == st.highlightedIndex
    then "background: cyan;"
    else ""

render :: State -> HTML
render state =
  HH.div_
  [ HH.slot _dropdown unit Select.component
    { render: renderSelect state
    , itemCount: Array.length state.items
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
    for_ (Array.index state.items index) \item ->
      H.modify_ $ _ { value = item }
    void $ H.query _dropdown unit Select.close
  Select.InputValueChanged value -> do
    H.modify_ $ _ { value = value }
  Select.VisibilityChanged _ -> pure unit
  Select.Emit q -> handleAction q
