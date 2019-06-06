module NSelect2
  ( module NSelect.Component
  , ExtraAction
  , Message(..)
  , Query
  , Slot
  , Action
  , HTML
  , State
  , component
  , raise
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import NSelect.Component (setRootProps, setToggleProps, setInputProps)
import NSelect.Component as NC
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element as Element
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event as Event
import Web.HTML as Web
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as ET

type Props item pa cs m =
  { render :: State -> HTML item pa cs m
  , items :: Array item
  }

data Message pa
  = Selected Int
  | InputValueChanged String
  | VisibilityChanged Boolean
  | Emit pa

type Query = Const Void
-- data Query a
--   = Open a
--   | Close a
--   | Focus a
--   | Highlight Int a
--   | Select a
--   | GetState (State -> a)

data ExtraAction item pa cs m
  = Receive (Props item pa cs m)
  | Raise pa

type Action item pa cs m
  = NC.Action (ExtraAction item pa cs m)

-- type InnerState pa cs m =
--   { props :: Props pa cs m
--   , clickedInside :: Boolean
--   , isOpen :: Boolean
--   , highlightedIndex :: Int
--   }

type HTML item pa cs m = H.ComponentHTML (Action item pa cs m) cs m

type DSL item pa cs m =
  H.HalogenM (InnerState item pa cs m) (Action item pa cs m) cs (Message pa) m

type Slot pa s = H.Slot Query (Message pa) s

type ExtraStateRow item pa cs m =
  ( props :: Props item pa cs m
  )

type InnerState item pa cs m = NC.State item (ExtraStateRow item pa cs m)

type State =
  { isOpen :: Boolean
  , highlightedIndex :: Int
  }

innerStateToState :: forall item pa cs m. InnerState item pa cs m -> State
innerStateToState { select: { isOpen, highlightedIndex } } =
  { isOpen
  , highlightedIndex
  }

initialState :: forall item pa cs m. Props item pa cs m -> InnerState item pa cs m
initialState props =
  { select: NC.initialState
  , items: props.items
  , props
  }

-- render :: forall pa cs m. InnerState pa cs m -> HTML pa cs m
-- render state =
--   state.props.render $ innerStateToState state

-- render :: forall item pa cs m. State item -> HTML pa cs m
render :: forall item pa cs m. InnerState item pa cs m -> HTML item pa cs m
render state =
  state.props.render $ innerStateToState state

component
  :: forall item pa cs m
   . MonadAff m
  => H.Component HH.HTML Query (Props item pa cs m) (Message pa) m
component = H.mkComponent
  { initialState
  , render
  -- , eval: H.mkEval $ H.defaultEval
  , eval: H.mkEval $ NC.defaultEval
      { handleAction = NC.handleAction handleAction handleSelectMessage
      , receive = Just <<< NC.ExtraAction <<< Receive
      -- , handleQuery = handleQuery
      -- , initialize = Just Init
      -- , receive = Just <<< ReceiveProps
      }
  }

-- handleAction :: forall item pa cs m. NC.Message -> DSL item pa cs m Unit
handleAction :: forall item pa cs m. ExtraAction item pa cs m -> DSL item pa cs m Unit
handleAction = case _ of
  Receive props -> H.modify_ $ _ { props = props }
  Raise pa -> do
    traceM "raise"
    H.raise $ Emit pa
  -- NC.ExtraAction a -> case a of
  -- q -> NC.handleAction q

handleSelectMessage :: forall item pa cs m. NC.Message -> DSL item pa cs m Unit
handleSelectMessage = case _ of
  _ -> pure unit

-- | Following are helpers so that you can query from the parent component.
raise :: forall item pa cs m. pa -> Action item pa cs m
-- raise = Raise <<< NC.ExtraAction
raise = NC.ExtraAction <<< Raise
