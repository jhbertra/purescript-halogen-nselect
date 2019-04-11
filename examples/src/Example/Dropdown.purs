module Example.Dropdown where

import Prelude

import Control.MonadPlus (guard)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import NSelect2 as Select

type Query = Const Void

data ExtraAction
  = OnInput String

type Action = Select.Action ExtraAction

type ExtraStateRow =
  ( value :: String
  )

type State = Select.State ExtraStateRow

type HTML = H.ComponentHTML Action () Aff

initialState :: Record ExtraStateRow
initialState =
  { value: ""
  }

render :: State -> HTML
render state =
  HH.div
  ( Select.setRootProps []
  ) $ join
  [ pure $ HH.div
    ( Select.setToggleProps [])
    [ HH.text "toggle" ]
  , guard state.select.isOpen $> HH.div_
    [ HH.input
      [ HP.value state.value
      , HE.onValueInput $ Just <<< Select.ExtraAction <<< OnInput
      ]
    , HH.div_
      [ HH.text $ "You typed: " <> state.value
      ]
    ]
  ]

component :: H.Component HH.HTML Query Unit Void Aff
component = Select.mkComponent
  { initialState: const $ Select.initialState initialState
  , render
  , handleAction
  }

handleAction :: ExtraAction -> H.HalogenM State Action () Void Aff Unit
handleAction (OnInput value) = do
  H.modify_ $ _ { value = value }
