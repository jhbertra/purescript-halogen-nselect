module Example.Dropdown where

import Prelude

import Control.MonadPlus (guard)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import NSelect as Select

data Query a
  = OnInput String a
  | HandleDropdown (Select.Message Query) a

type State =
  { value :: String
  }

type Slots =
  ( dropdown :: Select.Slot Query () Aff Unit
  )

_dropdown = SProxy :: SProxy "dropdown"

type HTML = H.ComponentHTML Query Slots Aff

type DSL = H.HalogenM State Query Slots Void Aff

initialState :: State
initialState =
  { value: ""
  }

renderSelect :: State -> Select.State -> Select.HTML Query () Aff
renderSelect state st =
  HH.div
  ( Select.setRootProps []
  ) $ join
  [ pure $ HH.div
    ( Select.setToggleProps [])
    [ HH.text "toggle" ]
  , guard st.isOpen $> HH.div_
    [ HH.input
      [ HP.value state.value
      , HE.onValueInput $ HE.input \v -> Select.raise $ OnInput v unit
      ]
    , HH.div_
      [ HH.text $ "You typed: " <> state.value
      ]
    ]
  ]

render :: State -> HTML
render state =
  HH.div_
  [ HH.slot _dropdown unit Select.component
    { render: renderSelect state
    , itemCount: 0
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
  eval (OnInput value n) = n <$ do
    H.modify_ $ _ { value = value }

  eval (HandleDropdown msg n) = n <$ do
    case msg of
      Select.Emit q -> eval q
      _ -> pure unit
