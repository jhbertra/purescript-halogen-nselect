module Example.Autocomplete where

import Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import NSelect as Select

data Query a
  = HandleDropdown (Select.Message Query) a

type State =
  { value :: String
  , items :: Array String
  }

type Slots =
  ( dropdown :: Select.Slot Query () Aff Unit
  )

_dropdown = SProxy :: SProxy "dropdown"

type HTML = H.ComponentHTML Query Slots Aff

type DSL = H.HalogenM State Query Slots Void Aff

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

renderSelect :: State -> Select.RenderState -> Select.HTML Query () Aff
renderSelect state st =
  HH.div
  ( Select.setRootProps []
  ) $ join
  [ pure $ HH.input
    ( Select.setInputProps
      [ HP.value state.value
      ]
    )
  , guard st.open $> HH.div_
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
    } $ HE.input HandleDropdown
  ]

component :: H.Component HH.HTML Query Unit Void Aff
component = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
  eval :: Query ~> DSL
  eval (HandleDropdown msg n) = n <$ case msg of
    Select.Selected index -> do
      state <- H.get
      for_ (Array.index state.items index) \item ->
        H.modify_ $ _ { value = item }
      void $ H.query _dropdown unit $ H.action Select.close
    Select.ValueChanged value -> do
      H.modify_ $ _ { value = value }
    Select.Focused -> pure unit
    Select.Emit q -> eval q
