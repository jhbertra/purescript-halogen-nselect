module Example.Dropdown where

import Example.Prelude

import Control.MonadPlus (guard)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import NSelect as Select
import Type.Proxy (Proxy(..))

data Action
  = OnInput String
  | HandleDropdown (Select.Message Action)

type State =
  { value :: String
  }

type Slots =
  ( dropdown :: Select.Slot Action Unit
  )

_dropdown = Proxy :: Proxy "dropdown"

type HTML = H.ComponentHTML Action Slots Aff

initialState :: State
initialState =
  { value: ""
  }

renderSelect :: State -> Select.State -> Select.HTML Action () Aff
renderSelect state st =
  HH.div
    ( Select.setRootProps [ class_ "inline-block" ]
    ) $ join
    [ pure $ HH.button
        (Select.setToggleProps [])
        [ HH.text $ if st.isOpen then "🐳️ Close" else "🐋️ Open" ]
    , guard st.isOpen $> HH.div
        [ class_ "Dropdown p-4"
        ]
        [ HH.input
            [ HP.value state.value
            , HP.placeholder "Type something"
            , HE.onValueInput $ Select.raise <<< OnInput
            ]
        , HH.div_
            [ HH.text $ "You typed: " <> state.value
            ]
        ]
    ]

render :: State -> HTML
render state =
  HH.slot _dropdown unit Select.component
    { render: renderSelect state
    , itemCount: 0
    } $ HandleDropdown

component :: forall q. H.Component q Unit Void Aff
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
  case msg of
    Select.Emit q -> handleAction q
    _ -> pure unit
