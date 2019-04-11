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
import NSelect2 as Select

type Query = Const Void

type Action
  = Select.Action Void

type ExtraStateRow =
  ( value :: String
  )

type State = Select.State String ExtraStateRow

type HTML = H.ComponentHTML Action () Aff

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
  , select: Select.initialState
  , items
  }

style :: forall r i. String -> HH.IProp ("style" :: String | r) i
style = HH.attr (HH.AttrName "style")

render :: State -> HTML
render state =
  HH.div
  ( Select.setRootProps []
  ) $ join
  [ pure $ HH.input
    ( Select.setInputProps
      [ HP.value state.value
      ]
    )
  , guard state.select.isOpen $> HH.div_
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
    if index == state.select.highlightedIndex
    then "background: cyan;"
    else ""

component :: H.Component HH.HTML Query Unit Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ Select.defaultEval
      { handleAction = Select.handleAction handleAction handleMessage
      }
  }

type DSL = H.HalogenM State Action () Void Aff

handleAction :: Void -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
  _ -> pure unit
-- handleAction (HandleDropdown msg) = case msg of
  -- Select.InputValueChanged value -> do
  --   H.modify_ $ _ { value = value }
  -- Select.VisibilityChanged _ -> pure unit
  -- Select.Emit q -> handleAction q

handleMessage :: Select.Message -> DSL Unit
handleMessage = case _ of
  Select.Selected index -> do
    state <- H.get
    for_ (Array.index state.items index) \item ->
      H.modify_ $ _ { value = item }
    -- void $ H.query _dropdown unit Select.close
  _ -> pure unit
