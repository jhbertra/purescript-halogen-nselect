module NSelect.Component where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Prim.Row (class Cons, class Lacks, class Union)
import Record as Record
import Type.Row (type (+))
import Web.Event.Event as Event
import Web.HTML as Web
import Web.HTML.Window as Window
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as ET

type State i r =
  { select :: SelectStateRow i
  , items :: Array i
  | r
  }

type SelectStateRow i =
  { isOpen :: Boolean
  , clickedInside :: Boolean
  , highlightedIndex :: Int
  }

initialState :: forall i. SelectStateRow i
initialState =
  { isOpen: false
  , clickedInside: false
  , highlightedIndex: 0
  }

data Message
  = Selected Int

data Action a
  = Init
  | OnWindowMouseDown
  | OnMouseDownRoot
  | OnMouseUpRoot
  | OnMouseDownToggle
  | ExtraAction a
  | OnFocusInput
  | OnKeyDownInput KE.KeyboardEvent
  -- | OnKeyDownInput' (KeyDownHandler pa) KE.KeyboardEvent
  | OnClickItem Int
  | OnMouseEnterItem Int
  | OnValueInput String
  -- | Raise pa

defaultEval = H.defaultEval
  { initialize = Just Init
  }

type DSL i s a cs m = H.HalogenM (State i s) (Action a) cs Void m

handleVisibilityChange :: forall i s a cs m. Boolean -> DSL i s a cs m Unit
handleVisibilityChange isOpen = do
  H.modify_ $ _ { select { isOpen = isOpen } }
  -- H.raise $ VisibilityChanged isOpen

handleAction
  :: forall i s a cs m
   . MonadAff m
  => (a -> DSL i s a cs m Unit)
  -> (Message -> DSL i s a cs m Unit)
  -> Action a
  -> DSL i s a cs m Unit
handleAction handleExtra handleMessage = case _ of
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

  OnFocusInput -> do
    handleVisibilityChange true

  OnKeyDownInput kbEvent -> do
    let event = KE.toEvent kbEvent
    case KE.key kbEvent of
      "ArrowUp" -> do
        H.liftEffect $ Event.preventDefault event
        H.modify_ \s -> s
          { select
              { highlightedIndex = max 0 (s.select.highlightedIndex - 1) }
          }
      "ArrowDown" -> do
        H.liftEffect $ Event.preventDefault event
        H.modify_ \s -> s
          { select
              { highlightedIndex =
                   min (Array.length s.items - 1) (s.select.highlightedIndex + 1)
              }
          }
      "Enter" -> H.gets _.select.highlightedIndex >>= handleMessage <<< Selected
      _ -> pure unit

  OnClickItem index -> do
    handleMessage $ Selected index

  OnMouseEnterItem index -> do
    H.modify_ $ _
      { select
          { highlightedIndex = index }
      }

  OnValueInput value -> do
    pure unit
    -- H.raise $ InputValueChanged value

  ExtraAction action -> do
    handleExtra action


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

type InputProps r =
  ( value :: String
  , onFocus :: FocusEvent
  , onKeyDown :: KE.KeyboardEvent
  , onInput :: Event.Event
  | r
  )

inputRef :: H.RefLabel
inputRef = H.RefLabel "__nselect_input"

sharedInputProps
  :: forall a r
   . Array (HH.IProp (InputProps r) (Action a))
sharedInputProps =
  [ HP.ref inputRef
  , HE.onFocus $ Just <<< const OnFocusInput
  , HE.onValueInput $ Just <<< OnValueInput
  ]

setInputProps
  :: forall a r
   . Array (HH.IProp (InputProps r) (Action a))
  -> Array (HH.IProp (InputProps r) (Action a))
setInputProps props = props <> sharedInputProps <>
  [ HE.onKeyDown $ Just <<< OnKeyDownInput
  ]

menuRef :: H.RefLabel
menuRef = H.RefLabel "__nselect_menu"

-- | Use `setMenuProps` so that after ArrowUp/ArrowDown, highlighted item will
-- | still be visible.
setMenuProps
  :: forall a r
   . Array (HH.IProp r (Action a))
  -> Array (HH.IProp r (Action a))
setMenuProps props = props <>
  [ HP.ref menuRef
  ]

type ItemProps r =
  ( onClick :: ME.MouseEvent
  , onMouseEnter :: ME.MouseEvent
  | r
  )

setItemProps
  :: forall a r
   . Int
  -> Array (HH.IProp (ItemProps r) (Action a))
  -> Array (HH.IProp (ItemProps r) (Action a))
setItemProps index props = props <>
  [ HH.attr (HH.AttrName "data-nselect-item") (show index)
  , HE.onClick $ Just <<< const (OnClickItem index)
  , HE.onMouseEnter $ Just <<< const (OnMouseEnterItem index)
  ]
