module NSelect
  ( Props
  , Message(..)
  , Query(..)
  , RenderState
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
  { render :: RenderState -> HTML pq cs m
  , itemCount :: Int
  }

data Message pq
  = Selected Int
  | ValueChanged String
  | Focused
  | Emit (pq Unit)

data Query pq cs m a
  = Init a
  | ReceiveProps (Props pq cs m) a
  | OnWindowMouseDown a
  | OnMouseDownRoot a
  | OnMouseUpRoot a
  | OnMouseDownToggle a
  | OnFocusInput a
  | OnKeyDownInput KE.KeyboardEvent a
  | OnKeyDownInput' (KeyDownHandler pq) KE.KeyboardEvent a
  | OnMouseDownItem Int a
  | OnMouseEnterItem Int a
  | OnValueInput String a
  | Close a
  | Focus a
  | Select a
  | Raise (pq Unit) a

type State pq cs m =
  { props :: Props pq cs m
  , clickedInside :: Boolean
  , open :: Boolean
  , highlightedIndex :: Int
  }

initialState :: forall pq cs m. Props pq cs m -> State pq cs m
initialState props =
  { props
  , clickedInside: false
  , open: false
  , highlightedIndex: 0
  }

type RenderState =
  { open :: Boolean
  , highlightedIndex :: Int
  }

stateToRenderState :: forall pq cs m. State pq cs m -> RenderState
stateToRenderState { open, highlightedIndex } =
  { open
  , highlightedIndex
  }

type HTML pq cs m = H.ComponentHTML (Query pq cs m) cs m

type DSL pq cs m = H.HalogenM (State pq cs m) (Query pq cs m) cs (Message pq) m

type Slot f cs m s = H.Slot (Query f cs m) (Message f) s

type RootProps r =
  ( onMouseDown :: ME.MouseEvent
  , onMouseUp :: ME.MouseEvent
  | r
  )

-- Click outside the root will set `open` to false.
setRootProps
  :: forall pq cs m r
   . Array (HH.IProp (RootProps r) (Query pq cs m Unit))
  -> Array (HH.IProp (RootProps r) (Query pq cs m Unit))
setRootProps props = props <>
  [ HE.onMouseDown $ HE.input_ OnMouseDownRoot
  , HE.onMouseUp $ HE.input_ OnMouseUpRoot
  ]

type ToggleProps r =
  ( onMouseDown :: ME.MouseEvent
  | r
  )

setToggleProps
  :: forall pq cs m r
   . Array (HH.IProp (ToggleProps r) (Query pq cs m Unit))
  -> Array (HH.IProp (ToggleProps r) (Query pq cs m Unit))
setToggleProps props = props <>
  [ HE.onMouseDown $ HE.input_ OnMouseDownToggle
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
   . Array (HH.IProp (InputProps r) (Query pq cs m Unit))
sharedInputProps =
  [ HP.ref inputRef
  , HE.onFocus $ HE.input_ OnFocusInput
  , HE.onValueInput $ HE.input OnValueInput
  ]

setInputProps
  :: forall pq cs m r
   . Array (HH.IProp (InputProps r) (Query pq cs m Unit))
  -> Array (HH.IProp (InputProps r) (Query pq cs m Unit))
setInputProps props = props <> sharedInputProps <>
  [ HE.onKeyDown $ HE.input OnKeyDownInput
  ]

type KeyDownHandler pq = KE.KeyboardEvent -> pq Unit

-- | setInputProps' does everything setInputProps does, but also pass the
-- | keyboardEvent back to the parent component, so the parent can handle more
-- | key bindings like Tab.
setInputProps'
  :: forall pq cs m r
   . { onKeyDown :: KeyDownHandler pq }
  -> Array (HH.IProp (InputProps r) (Query pq cs m Unit))
  -> Array (HH.IProp (InputProps r) (Query pq cs m Unit))
setInputProps' parentHandlers props = props <> sharedInputProps <>
  [ HE.onKeyDown $ HE.input $ OnKeyDownInput' parentHandlers.onKeyDown
  ]

type ItemProps r =
  ( onMouseDown :: ME.MouseEvent
  , onMouseEnter :: ME.MouseEvent
  | r
  )

setItemProps
  :: forall pq cs m r
   . Int
  -> Array (HH.IProp (ItemProps r) (Query pq cs m Unit))
  -> Array (HH.IProp (ItemProps r) (Query pq cs m Unit))
setItemProps index props = props <>
  [ HE.onMouseDown $ HE.input_ $ OnMouseDownItem index
  , HE.onMouseEnter $ HE.input_ $ OnMouseEnterItem index
  ]

render :: forall pq cs m. State pq cs m -> HTML pq cs m
render state =
  state.props.render $ stateToRenderState state

component
  :: forall pq cs m
   . MonadAff m
  => H.Component HH.HTML (Query pq cs m) (Props pq cs m) (Message pq) m
component = H.component
  { initialState
  , render
  , eval
  , receiver: HE.input ReceiveProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where
  eval :: Query pq cs m ~> DSL pq cs m
  eval (Init n) = n <$ do
    win <- H.liftEffect Web.window
    H.subscribe $
      ES.eventListenerEventSource ET.mousedown (Window.toEventTarget win)
        (const $ Just $ H.action OnWindowMouseDown)

  eval (ReceiveProps props n) = n <$ do
    H.modify_ $ _ { props = props }

  eval (OnWindowMouseDown n) = n <$ do
    state <- H.get
    when (not state.clickedInside && state.open) $
      H.modify_ $ _ { open = false }

  eval (OnMouseDownRoot n) = n <$ do
    H.modify_ $ _ { clickedInside = true }

  eval (OnMouseUpRoot n) = n <$ do
    H.modify_ $ _ { clickedInside = false }

  eval (OnMouseDownToggle n) = n <$ do
    H.modify_ \s -> s { open = not s.open }

  eval (OnFocusInput n) = n <$ do
    H.modify_ $ _ { open = true }
    H.raise Focused

  eval (OnKeyDownInput kbEvent n) = n <$ do
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

  eval (OnKeyDownInput' parentOnKeyDown kbEvent n) = n <$ do
    eval (OnKeyDownInput kbEvent unit)
    H.raise $ Emit $ parentOnKeyDown kbEvent

  eval (OnMouseDownItem index n) = n <$ do
    H.raise $ Selected index

  eval (OnMouseEnterItem index n) = n <$ do
    H.modify_ $ _
      { highlightedIndex = index
      }

  eval (OnValueInput value n) = n <$ do
    H.raise $ ValueChanged value

  eval (Close n) = n <$ do
    H.modify_ $ _ { open = false }

  eval (Focus n) = n <$ do
    H.getHTMLElementRef inputRef >>= traverse_ \el -> do
      H.liftAff $ Aff.delay $ Aff.Milliseconds 0.0
      H.liftEffect $ HTMLElement.focus el

  eval (Select n) = n <$ do
    H.gets _.highlightedIndex >>= H.raise <<< Selected

  eval (Raise pq n) = n <$ do
    H.raise $ Emit pq


-- | Following are helpers so that you can query from the parent component.
-- | Query(..) are exposed in case you want to override the whole
-- | `setInputProps` behavior. Normally these helpers are enough.
close :: forall pq cs m. Query pq cs m Unit
close = Close unit

focus :: forall pq cs m. Query pq cs m Unit
focus = Focus unit

raise :: forall pq cs m a. pq Unit -> a -> Query pq cs m a
raise f = Raise f

select :: forall pq cs m . Query pq cs m Unit
select = Select unit
