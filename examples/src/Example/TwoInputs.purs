module Example.TwoInputs where

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
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KE

data Query a
  = OnKeyDownInput DropdownSlot KE.KeyboardEvent a
  | HandleDropdown DropdownSlot (Select.Message Query) a

type State =
  { from :: String
  , to :: String
  , items :: Array String
  }

data DropdownSlot = DropdownFrom | DropdownTo

derive instance eqDropdownSlot :: Eq DropdownSlot
derive instance ordDropdownSlot :: Ord DropdownSlot

type Slots =
  ( dropdown :: Select.Slot Query Aff DropdownSlot
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
  { from: ""
  , to: ""
  , items
  }

style :: forall r i. String -> HH.IProp ("style" :: String | r) i
style = HH.attr (HH.AttrName "style")

renderSelect :: State -> DropdownSlot -> Select.RenderState -> Select.HTML Query Aff
renderSelect state slot st =
  HH.div
  ( Select.setRootProps []
  ) $ join
  [ pure $ HH.input
    ( Select.setInputProps'
      { onKeyDown: \e -> OnKeyDownInput slot e unit
      }
      [ HP.value value
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
  value = case slot of
    DropdownFrom -> state.from
    DropdownTo -> state.to
  getStyle index =
    if index == st.highlightedIndex
    then "background: cyan;"
    else ""

render :: State -> HTML
render state =
  HH.div
  [ style "display: flex;"]
  [ HH.slot _dropdown DropdownFrom Select.component
    { render: renderSelect state DropdownFrom
    , itemCount: Array.length state.items
    } $ HE.input $ HandleDropdown DropdownFrom
  , HH.slot _dropdown DropdownTo Select.component
    { render: renderSelect state DropdownTo
    , itemCount: Array.length state.items
    } $ HE.input $ HandleDropdown DropdownTo
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
  eval (OnKeyDownInput slot kbEvent n) = n <$ do
    let event = KE.toEvent kbEvent
    case KE.key kbEvent of
      "Tab" -> do
        H.liftEffect $ Event.preventDefault event
        void $ H.query _dropdown slot $ H.action Select.select
      _ -> pure unit

  eval (HandleDropdown slot msg n) = n <$ case msg of
    Select.Selected index -> do
      state <- H.get
      for_ (Array.index state.items index) \item ->
        case slot of
          DropdownFrom -> H.modify_ $ _ { from = item }
          DropdownTo -> H.modify_ $ _ { to = item }
      void $ H.query _dropdown slot $ H.action Select.close
      when (slot == DropdownFrom) $ do
        void $ H.query _dropdown DropdownTo $ H.action Select.focus
    Select.ValueChanged value -> do
      case slot of
        DropdownFrom -> H.modify_ $ _ { from = value }
        DropdownTo -> H.modify_ $ _ { to = value }
    Select.Emit q -> eval q
