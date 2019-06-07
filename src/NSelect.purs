module NSelect
  ( module NSelect.Component
  , ExtraAction
  , Query'
  , Message(..)
  , Slot
  , Action
  , HTML
  , component
  , raise
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import NSelect.Component (Query(..), State, setRootProps, setToggleProps, setInputProps, setInputProps', setMenuProps, setItemProps)
import NSelect.Component as SC

type Props pa cs m =
  { render :: SC.State -> HTML pa cs m
  , itemCount :: Int
  }

data Message pa
  = Selected Int
  | InputValueChanged String
  | VisibilityChanged Boolean
  | Emit pa

type Query' = SC.Query

data ExtraAction pa cs m
  = Receive (Props pa cs m)
  | Raise pa

type Action pa cs m
  = SC.Action (ExtraAction pa cs m)

type HTML pa cs m = H.ComponentHTML (Action pa cs m) cs m

type DSL pa cs m =
  H.HalogenM (InnerState pa cs m) (Action pa cs m) cs (Message pa) m

type Slot pa s = H.Slot Query' (Message pa) s

type ExtraStateRow pa cs m =
  ( props :: Props pa cs m
  )

type InnerState pa cs m = SC.InnerState (ExtraStateRow pa cs m)

initialState :: forall pa cs m. Props pa cs m -> InnerState pa cs m
initialState props =
  { select: SC.initialState
  , getItemCount: \s -> s.props.itemCount
  , props
  }

render :: forall pa cs m. InnerState pa cs m -> HTML pa cs m
render state =
  state.props.render $ SC.selectStateToState state.select

component
  :: forall pa cs m
   . MonadAff m
  => H.Component HH.HTML Query' (Props pa cs m) (Message pa) m
component = H.mkComponent
  { initialState
  , render
  , eval: SC.mkEval $ SC.defaultEval
      { receive = Just <<< SC.ExtraAction <<< Receive
      , handleAction = handleAction
      , handleMessage = handleSelectMessage
      }
  }

handleAction :: forall pa cs m. ExtraAction pa cs m -> DSL pa cs m Unit
handleAction = case _ of
  Receive props -> H.modify_ $ _ { props = props }
  Raise pa -> H.raise $ Emit pa

handleSelectMessage :: forall pa cs m. SC.Message -> DSL pa cs m Unit
handleSelectMessage = case _ of
  SC.Selected v -> H.raise $ Selected v
  SC.InputValueChanged v -> H.raise $ InputValueChanged v
  SC.VisibilityChanged v -> H.raise $ VisibilityChanged v

-- | Following are helpers so that you can query from the parent component.
raise :: forall pa cs m. pa -> Action pa cs m
raise = SC.ExtraAction <<< Raise
