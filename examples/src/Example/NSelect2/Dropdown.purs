module Example.NSelect2.Dropdown where

import Example.Prelude

import Control.MonadPlus (guard)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import NSelect2 as Select

type Query = Const Void

data Action
  = OnInput String
  | HandleDropdown (Select.Output Action)

type State =
  { value :: String
  }

type Slots =
  ( dropdown :: Select.Slot Action Unit
  )

_dropdown = SProxy :: SProxy "dropdown"

type HTML = H.ComponentHTML Action Slots Aff

initialState :: State
initialState =
  { value: ""
  }

renderSelect :: State -> Select.State -> Select.HTML String Action () Aff
renderSelect state st =
  HH.div
  ( Select.setRootProps [ class_ "inline-block"]
  ) $ join
  [ pure $ HH.button
    ( Select.setToggleProps [])
    [ HH.text "toggle" ]
  , guard st.isOpen $> HH.div
    [ class_ "Dropdown p-4"
    ]
    [ HH.input
      [ HP.value state.value
      , HE.onValueInput $ Just <<< Select.raise <<< OnInput
      ]
    , HH.div_
      [ HH.text $ "You typed: " <> state.value
      ]
    ]
  ]

render :: State -> HTML
render state =
  HH.div_
  [ HH.p
    [ class_ "mb-3"]
    [ HH.text "Trigger parent action from dropdown."]
  , HH.slot _dropdown unit Select.component
    { render: renderSelect state
    -- , itemCount: 0
    , items: []
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
handleAction (OnInput value) = do
  H.modify_ $ _ { value = value }
handleAction (HandleDropdown msg) = do
  traceM msg
  case msg of
    Select.Emit q -> do
      handleAction q
    _ -> pure unit
