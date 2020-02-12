module Example.ComponentInDropdown.Child where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Query = Const Void

type Message = String

data Action
  = OnInput String

type State =
  { value :: String
  }

type HTML = H.ComponentHTML Action () Aff

initialState :: State
initialState =
  { value: ""
  }

render :: State -> HTML
render state =
  HH.div_
  [ HH.input
    [ HP.value state.value
    , HP.placeholder "Type something"
    , HE.onValueInput $ Just <<< OnInput
    ]
  ]

component :: H.Component HH.HTML Query Unit Message Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
handleAction (OnInput value) = do
  H.modify_ $ _ { value = value }
  H.raise value
