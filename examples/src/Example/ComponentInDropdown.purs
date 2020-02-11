module Example.ComponentInDropdown where

import Example.Prelude

import Control.MonadPlus (guard)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Example.ComponentInDropdown.Child as Child
import Halogen as H
import Halogen.HTML as HH
import NSelect as Select

type Query = Const Void

data Action
  = HandleDropdown (Select.Message Action)
  | HandleChild Child.Message

type State =
  { value :: String
  }

type Slots =
  ( dropdown :: Select.Slot Action Unit
  )

type SelectSlots =
  ( child :: H.Slot Child.Query Child.Message Unit
  )

_dropdown = SProxy :: SProxy "dropdown"
_child = SProxy :: SProxy "child"

type HTML = H.ComponentHTML Action Slots Aff

initialState :: State
initialState =
  { value: ""
  }

renderSelect :: State -> Select.State -> Select.HTML Action SelectSlots Aff
renderSelect state st =
  HH.div
  ( Select.setRootProps
    [ class_ "inline-block" ]
  ) $ join
  [ pure $ HH.button
    ( Select.setToggleProps [])
    [ HH.text "Toggle" ]
  , guard st.isOpen $>
      HH.div
      [ class_ "Dropdown p-4"
      ]
      [ HH.slot _child unit Child.component unit
          (Just <<< Select.raise <<< HandleChild)
      ]
  ]

render :: State -> HTML
render state =
  HH.div_
  [ HH.slot _dropdown unit Select.component
    { render: renderSelect state
    , itemCount: 0
    } $ Just <<< HandleDropdown
  , HH.div_
    [ HH.text $ "You typed: " <> state.value
    ]
  ]

component :: H.Component HH.HTML Query Unit Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction = case _ of
  HandleDropdown msg -> do
    case msg of
      Select.Emit q -> handleAction q
      _ -> pure unit
  HandleChild value -> do
    H.modify_ $ _ { value = value }
