module Example.TwoInputs where

import Example.Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import NSelect as Select
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KE

type Query = Const Void

data Action
  = OnKeyDownInput DropdownSlot KE.KeyboardEvent
  | HandleDropdown DropdownSlot (Select.Message Action)

type State =
  { from :: String
  , to :: String
  , items :: Array String
  }

data DropdownSlot = DropdownFrom | DropdownTo

derive instance eqDropdownSlot :: Eq DropdownSlot
derive instance ordDropdownSlot :: Ord DropdownSlot

type Slots =
  ( dropdown :: Select.Slot Action DropdownSlot
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
  { from: ""
  , to: ""
  , items
  }

renderSelect
  :: State
  -> DropdownSlot
  -> Select.State
  -> Select.HTML Action () Aff
renderSelect state slot st =
  HH.div
  ( Select.setRootProps []
  ) $ join
  [ pure $ HH.input
    ( Select.setInputProps'
      { onKeyDown: \e -> OnKeyDownInput slot e
      }
      [ HP.value value
      ]
    )
  , guard st.isOpen $> HH.div
    [ class_ "shadow-md absolute"
    , style "width: 20rem;"
    ]
    [ HH.ul_ $
        state.items # Array.mapWithIndex \index item ->
      HH.li
      ( Select.setItemProps index
        [ class_ $ "py-1 px-3 cursor-pointer" <>
            Monoid.guard (index == st.highlightedIndex) " bg-blue-300"
        ]
      )
      [ HH.text item ]
    ]
  ]
  where
  value = case slot of
    DropdownFrom -> state.from
    DropdownTo -> state.to

render :: State -> HTML
render state =
  HH.div_
  [ HH.p
    [ class_ "mb-3"]
    [ HH.text "Use TAB to complete the first and focus the second input."]
  , HH.div
    [ class_ "flex" ]
    [ HH.slot _dropdown DropdownFrom Select.component
      { render: renderSelect state DropdownFrom
      , itemCount: Array.length state.items
      } $ Just <<< HandleDropdown DropdownFrom
    , HH.span
      [ class_ "mx-4" ]
      [ HH.text "-" ]
    , HH.slot _dropdown DropdownTo Select.component
      { render: renderSelect state DropdownTo
      , itemCount: Array.length state.items
      } $ Just <<< HandleDropdown DropdownTo
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
handleAction (OnKeyDownInput slot kbEvent) = do
  let event = KE.toEvent kbEvent
  case KE.key kbEvent of
    "Tab" -> do
      H.liftEffect $ Event.preventDefault event
      void $ H.query _dropdown slot Select.select
    _ -> pure unit
handleAction (HandleDropdown slot msg) = case msg of
  Select.Selected index -> do
    state <- H.get
    for_ (Array.index state.items index) \item ->
      case slot of
        DropdownFrom -> H.modify_ $ _ { from = item }
        DropdownTo -> H.modify_ $ _ { to = item }
    void $ H.query _dropdown slot Select.close
    when (slot == DropdownFrom) $ do
      void $ H.query _dropdown DropdownTo Select.focus
  Select.InputValueChanged value -> do
    case slot of
      DropdownFrom -> H.modify_ $ _ { from = value }
      DropdownTo -> H.modify_ $ _ { to = value }
  Select.VisibilityChanged _ -> pure unit
  Select.Emit q -> handleAction q
