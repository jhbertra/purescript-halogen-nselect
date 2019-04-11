module NSelect2 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES
import Prim.Row (class Lacks, class Union)
import Record as Record
import Type.Row (type (+))
import Web.HTML as Web
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as ET

type State r =
  { select ::
      { isOpen :: Boolean
      , clickedInside :: Boolean
      , highlightedIndex :: Int
      }
  | r
  }

initialState
  :: forall s
   . Lacks "select" s
  => Record s
  -> State s
initialState state =
  Record.insert (SProxy :: SProxy "select")
    { isOpen: false
    , clickedInside: false
    , highlightedIndex: 0
    } state

data Action a
  = Init
  | ExtraAction a
  | OnWindowMouseDown
  | OnMouseDownRoot
  | OnMouseUpRoot
  | OnMouseDownToggle
  -- | OnFocusInput
  -- | OnKeyDownInput KE.KeyboardEvent
  -- | OnKeyDownInput' (KeyDownHandler pa) KE.KeyboardEvent
  -- | OnClickItem Int
  -- | OnMouseEnterItem Int
  -- | OnValueInput String
  -- | Raise pa

mkComponent config@{ initialState, render } = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction config.handleAction
      , initialize = Just Init
      }
      -- , handleQuery = handleQuery
      -- , receive = Just <<< ReceiveProps
  }

type DSL s a cs m = H.HalogenM (State s) (Action a) cs Void m

handleVisibilityChange :: forall s a cs m. Boolean -> DSL s a cs m Unit
handleVisibilityChange isOpen = do
  H.modify_ $ _ { select { isOpen = isOpen } }
  -- H.raise $ VisibilityChanged isOpen

handleAction
  :: forall s a cs m
   . MonadAff m
  => (a -> DSL s a cs m Unit)
  -> Action a
  -> DSL s a cs m Unit
handleAction handleExtra = case _ of
  Init -> do
    win <- H.liftEffect Web.window
    void $ H.subscribe $
      ES.eventListenerEventSource ET.mousedown (Window.toEventTarget win)
        (const $ Just OnWindowMouseDown)

  OnWindowMouseDown -> do
    { select } <- H.get
    when (not select.clickedInside && select.isOpen) $ do
      handleVisibilityChange false

  OnMouseDownRoot -> do
    H.modify_ $ _ { select { clickedInside = true } }

  OnMouseUpRoot -> do
    H.modify_ $ _ { select { clickedInside = false } }

  OnMouseDownToggle -> do
    { select } <- H.get
    handleVisibilityChange $ not select.isOpen

  ExtraAction action -> do
    handleExtra action
  _ -> pure unit


type RootProps r =
  ( onMouseDown :: ME.MouseEvent
  , onMouseUp :: ME.MouseEvent
  | r
  )

-- Click outside the root will close the dropdown.
setRootProps
  :: forall a r
   . Array (HH.IProp (RootProps r) (Action a))
  -> Array (HH.IProp (RootProps r) (Action a))
setRootProps props = props <>
  [ HE.onMouseDown $ Just <<< const OnMouseDownRoot
  , HE.onMouseUp $ Just <<< const OnMouseUpRoot
  ]

type ToggleProps r =
  ( onMouseDown :: ME.MouseEvent
  | r
  )

setToggleProps
  :: forall a r
   . Array (HH.IProp (ToggleProps r) (Action a))
  -> Array (HH.IProp (ToggleProps r) (Action a))
setToggleProps props = props <>
  [ HE.onMouseDown $ Just <<< const OnMouseDownToggle
  ]
