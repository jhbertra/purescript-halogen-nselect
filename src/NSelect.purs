module NSelect
  ( Props
  , Message(..)
  , Query(..)
  , Action(..)
  , State
  , HTML
  , Slot
  , KeyDownHandler
  , setRootProps
  , setToggleProps
  , setInputProps
  , setInputProps'
  , setItemProps
  , component
  , close
  , focus
  , raise
  , select
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Web.Event.Event as Event
import Web.HTML as Web
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as ET

type Props pq cs m =
  { render :: State -> HTML pq cs m
  , itemCount :: Int
  }

data Message pq
  = Selected Int
  | InputValueChanged String
  | VisibilityChanged Boolean
  | Emit pq

data Query a
  = Open a
  | Close a
  | Focus a
  | Select a

data Action pq cs m
  = Init
  | ReceiveProps (Props pq cs m)
  | OnWindowMouseDown
  | OnMouseDownRoot
  | OnMouseUpRoot
  | OnMouseDownToggle
  | OnFocusInput
  | OnKeyDownInput KE.KeyboardEvent
  | OnKeyDownInput' (KeyDownHandler pq) KE.KeyboardEvent
  | OnClickItem Int
  | OnMouseEnterItem Int
  | OnValueInput String
  | Raise pq

type InnerState pq cs m =
  { props :: Props pq cs m
  , clickedInside :: Boolean
  , isOpen :: Boolean
  , highlightedIndex :: Int
  }

initialState :: forall pq cs m. Props pq cs m -> InnerState pq cs m
initialState props =
  { props
  , clickedInside: false
  , isOpen: false
  , highlightedIndex: 0
  }

type State =
  { isOpen :: Boolean
  , highlightedIndex :: Int
  }

innerStateToState :: forall pq cs m. InnerState pq cs m -> State
innerStateToState { isOpen, highlightedIndex } =
  { isOpen
  , highlightedIndex
  }

type HTML pq cs m = H.ComponentHTML (Action pq cs m) cs m

type DSL pq cs m = H.HalogenM (InnerState pq cs m) (Action pq cs m) cs (Message pq) m

type Slot f s = H.Slot Query (Message f) s

type RootProps r =
  ( onMouseDown :: ME.MouseEvent
  , onMouseUp :: ME.MouseEvent
  | r
  )

-- Click outside the root will close the dropdown.
setRootProps
  :: forall pq cs m r
   . Array (HH.IProp (RootProps r) (Action pq cs m))
  -> Array (HH.IProp (RootProps r) (Action pq cs m))
setRootProps props = props <>
  [ HE.onMouseDown $ Just <<< const OnMouseDownRoot
  , HE.onMouseUp $ Just <<< const OnMouseUpRoot
  ]

type ToggleProps r =
  ( onMouseDown :: ME.MouseEvent
  | r
  )

setToggleProps
  :: forall pq cs m r
   . Array (HH.IProp (ToggleProps r) (Action pq cs m))
  -> Array (HH.IProp (ToggleProps r) (Action pq cs m))
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
  :: forall pq cs m r
   . Array (HH.IProp (InputProps r) (Action pq cs m))
sharedInputProps =
  [ HP.ref inputRef
  , HE.onFocus $ Just <<< const OnFocusInput
  , HE.onValueInput $ Just <<< OnValueInput
  ]

setInputProps
  :: forall pq cs m r
   . Array (HH.IProp (InputProps r) (Action pq cs m))
  -> Array (HH.IProp (InputProps r) (Action pq cs m))
setInputProps props = props <> sharedInputProps <>
  [ HE.onKeyDown $ Just <<< OnKeyDownInput
  ]

type KeyDownHandler pq = KE.KeyboardEvent -> pq

-- | setInputProps' does everything setInputProps does, but also pass the
-- | keyboardEvent back to the parent component, so the parent can handle more
-- | key bindings like Tab.
setInputProps'
  :: forall pq cs m r
   . { onKeyDown :: KeyDownHandler pq }
  -> Array (HH.IProp (InputProps r) (Action pq cs m))
  -> Array (HH.IProp (InputProps r) (Action pq cs m))
setInputProps' parentHandlers props = props <> sharedInputProps <>
  [ HE.onKeyDown $ Just <<< OnKeyDownInput' parentHandlers.onKeyDown
  ]

type ItemProps r =
  ( onClick :: ME.MouseEvent
  , onMouseEnter :: ME.MouseEvent
  | r
  )

setItemProps
  :: forall pq cs m r
   . Int
  -> Array (HH.IProp (ItemProps r) (Action pq cs m))
  -> Array (HH.IProp (ItemProps r) (Action pq cs m))
setItemProps index props = props <>
  [ HE.onClick $ Just <<< const (OnClickItem index)
  , HE.onMouseEnter $ Just <<< const (OnMouseEnterItem index)
  ]

render :: forall pq cs m. InnerState pq cs m -> HTML pq cs m
render state =
  state.props.render $ innerStateToState state

component
  :: forall pq cs m
   . MonadAff m
  => H.Component HH.HTML Query (Props pq cs m) (Message pq) m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Init
      , receive = Just <<< ReceiveProps
      }
  }

handleVisibilityChange :: forall pq cs m. Boolean -> DSL pq cs m Unit
handleVisibilityChange isOpen = do
  H.modify_ $ _ { isOpen = isOpen }
  H.raise $ VisibilityChanged isOpen

handleAction
  :: forall pq cs m
   . MonadAff m
  => Action pq cs m
  -> DSL pq cs m Unit
handleAction = case _ of
  Init -> do
    win <- H.liftEffect Web.window
    void $ H.subscribe $
      ES.eventListenerEventSource ET.mousedown (Window.toEventTarget win)
        (const $ Just OnWindowMouseDown)

  ReceiveProps props -> do
    H.modify_ $ _ { props = props }

  OnWindowMouseDown -> do
    state <- H.get
    when (not state.clickedInside && state.isOpen) $ do
      handleVisibilityChange false

  OnMouseDownRoot -> do
    H.modify_ $ _ { clickedInside = true }

  OnMouseUpRoot -> do
    H.modify_ $ _ { clickedInside = false }

  OnMouseDownToggle -> do
    state <- H.get
    handleVisibilityChange $ not state.isOpen

  OnFocusInput -> do
    handleVisibilityChange true

  OnKeyDownInput kbEvent -> do
    let event = KE.toEvent kbEvent
    case KE.key kbEvent of
      "ArrowUp" -> do
        H.liftEffect $ Event.preventDefault event
        H.modify_ \s -> s
          { highlightedIndex = max 0 (s.highlightedIndex - 1) }
      "ArrowDown" -> do
        H.liftEffect $ Event.preventDefault event
        H.modify_ \s -> s
          { highlightedIndex =
            min (s.props.itemCount - 1) (s.highlightedIndex + 1)
          }
      "Enter" -> H.gets _.highlightedIndex >>= H.raise <<< Selected
      _ -> pure unit

  OnKeyDownInput' parentOnKeyDown kbEvent -> do
    handleAction (OnKeyDownInput kbEvent)
    H.raise $ Emit $ parentOnKeyDown kbEvent

  OnClickItem index -> do
    H.raise $ Selected index

  OnMouseEnterItem index -> do
    H.modify_ $ _
      { highlightedIndex = index
      }

  OnValueInput value -> do
    H.raise $ InputValueChanged value

  Raise pq -> do
    H.raise $ Emit pq


handleQuery
  :: forall pq cs m a
   . MonadAff m
  => Query a
  -> DSL pq cs m (Maybe a)
handleQuery = case _ of
  Open n -> do
    handleVisibilityChange true
    pure $ Just n

  Close n -> do
    handleVisibilityChange false
    pure $ Just n

  Focus n -> do
    H.getHTMLElementRef inputRef >>= traverse_ \el -> do
      H.liftAff $ Aff.delay $ Aff.Milliseconds 0.0
      H.liftEffect $ HTMLElement.focus el
    pure $ Just n

  Select n -> do
    H.gets _.highlightedIndex >>= H.raise <<< Selected
    pure $ Just n

-- | Following are helpers so that you can query from the parent component.
-- | Query(..) are exposed in case you want to override the whole
-- | `setInputProps` behavior. Normally these helpers are enough.
open :: Query Unit
open = Open unit

close :: Query Unit
close = Close unit

focus :: Query Unit
focus = Focus unit

raise :: forall pq cs m. pq -> Action pq cs m
raise f = Raise f

select :: Query Unit
select = Select unit
