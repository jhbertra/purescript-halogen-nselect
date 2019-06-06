module NSelect2
  ( module NSelect.Component
  , ExtraAction
  , Query'
  , Slot
  , Action
  , HTML
  , State
  , component
  , raise
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import NSelect.Component (Query(..), Message(..), setRootProps, setToggleProps, setInputProps, setInputProps', setMenuProps, setItemProps)
import NSelect.Component as SC

type Props item pa cs m =
  { render :: State -> HTML item pa cs m
  , items :: Array item
  }

type Query' = SC.Query Void

data ExtraAction item pa cs m
  = Receive (Props item pa cs m)
  | Raise pa

type Action item pa cs m
  = SC.Action (ExtraAction item pa cs m)

type HTML item pa cs m = H.ComponentHTML (Action item pa cs m) cs m

type DSL item pa cs m =
  H.HalogenM (InnerState item pa cs m) (Action item pa cs m) cs (Message pa) m

type Slot pa s = H.Slot Query' (Message pa) s

type ExtraStateRow item pa cs m =
  ( props :: Props item pa cs m
  )

type InnerState item pa cs m = SC.State item (ExtraStateRow item pa cs m)

type State =
  { isOpen :: Boolean
  , highlightedIndex :: Int
  }

initialState :: forall item pa cs m. Props item pa cs m -> InnerState item pa cs m
initialState props =
  { select: SC.initialState
  , items: props.items
  , props
  }

render :: forall item pa cs m. InnerState item pa cs m -> HTML item pa cs m
render state =
  state.props.render $ SC.selectStateToRenderState state.select

component
  :: forall item pa cs m
   . MonadAff m
  => H.Component HH.HTML Query' (Props item pa cs m) (Message pa) m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ SC.defaultEval
      -- { handleAction = SC.handleAction handleAction handleSelectMessage
      { handleAction = SC.handleAction handleAction H.raise
      -- , handleQuery = SC.handleQuery H.raise
      -- , handleQuery = SC.handleQuery ?h
      , receive = Just <<< SC.ExtraAction <<< Receive
      }
  }

handleAction :: forall item pa cs m. ExtraAction item pa cs m -> DSL item pa cs m Unit
handleAction = case _ of
  Receive props -> H.modify_ $ _ { props = props }
  Raise pa -> do
    H.raise $ Emit pa

-- handleSelectMessage :: forall item pa pa' cs m. SC.Message pa' -> DSL item pa cs m Unit
handleSelectMessage :: forall item pa cs m. SC.Message pa -> DSL item pa cs m Unit
-- handleSelectMessage = H.raise
handleSelectMessage = case _ of
  Emit _ -> pure unit
  Selected v -> H.raise $ Selected v
  InputValueChanged v -> H.raise $ InputValueChanged v
  VisibilityChanged v -> H.raise $ VisibilityChanged v

-- | Following are helpers so that you can query from the parent component.
raise :: forall item pa cs m. pa -> Action item pa cs m
raise = SC.ExtraAction <<< Raise
