module NSelect
  ( Props
  , Message(..)
  , Query
  , Action
  , State
  , HTML
  , Slot
  , KeyDownHandler
  , setRootProps
  , setToggleProps
  , setInputProps
  , setInputProps'
  , setMenuProps
  , setItemProps
  , component
  , open
  , close
  , focus
  , highlight
  , raise
  , select
  , getState
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
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

type Props pa cs m =
  { render :: State -> HTML pa cs m
  , itemCount :: Int
  }

data Message pa
  = Selected Int
  | InputValueChanged String
  | VisibilityChanged Boolean
  | Emit pa

data Query a
  = Open a
  | Close a
  | Focus a
  | Highlight Int a
  | Select a
  | GetState (State -> a)

data Action pa cs m
  = Init
  | ReceiveProps (Props pa cs m)
  | OnWindowMouseDown
  | OnMouseDownRoot
  | OnMouseUpRoot
  | OnMouseDownToggle
  | OnFocusInput
  | OnKeyDownInput KE.KeyboardEvent
  | OnKeyDownInput' (KeyDownHandler pa) KE.KeyboardEvent
  | OnClickItem Int
  | OnMouseEnterItem Int
  | OnValueInput String
  | Raise pa

type InnerState pa cs m =
  { props :: Props pa cs m
  , clickedInside :: Boolean
  , isOpen :: Boolean
  , highlightedIndex :: Int
  }

initialState :: forall pa cs m. Props pa cs m -> InnerState pa cs m
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

innerStateToState :: forall pa cs m. InnerState pa cs m -> State
innerStateToState { isOpen, highlightedIndex } =
  { isOpen
  , highlightedIndex
  }

type HTML pa cs m = H.ComponentHTML (Action pa cs m) cs m

type DSL pa cs m = H.HalogenM (InnerState pa cs m) (Action pa cs m) cs (Message pa) m

type Slot f s = H.Slot Query (Message f) s

type RootProps r =
  ( onMouseDown :: ME.MouseEvent
  , onMouseUp :: ME.MouseEvent
  | r
  )

-- Click outside the root will close the dropdown.
setRootProps
  :: forall pa cs m r
   . Array (HH.IProp (RootProps r) (Action pa cs m))
  -> Array (HH.IProp (RootProps r) (Action pa cs m))
setRootProps props = props <>
  [ HE.onMouseDown $ Just <<< const OnMouseDownRoot
  , HE.onMouseUp $ Just <<< const OnMouseUpRoot
  ]

type ToggleProps r =
  ( onMouseDown :: ME.MouseEvent
  | r
  )

setToggleProps
  :: forall pa cs m r
   . Array (HH.IProp (ToggleProps r) (Action pa cs m))
  -> Array (HH.IProp (ToggleProps r) (Action pa cs m))
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
  :: forall pa cs m r
   . Array (HH.IProp (InputProps r) (Action pa cs m))
sharedInputProps =
  [ HP.ref inputRef
  , HE.onFocus $ Just <<< const OnFocusInput
  , HE.onValueInput $ Just <<< OnValueInput
  ]

setInputProps
  :: forall pa cs m r
   . Array (HH.IProp (InputProps r) (Action pa cs m))
  -> Array (HH.IProp (InputProps r) (Action pa cs m))
setInputProps props = props <> sharedInputProps <>
  [ HE.onKeyDown $ Just <<< OnKeyDownInput
  ]

type KeyDownHandler pa = KE.KeyboardEvent -> pa

-- | setInputProps' does everything setInputProps does, but also pass the
-- | keyboardEvent back to the parent component, so the parent can handle more
-- | key bindings like Tab.
setInputProps'
  :: forall pa cs m r
   . { onKeyDown :: KeyDownHandler pa }
  -> Array (HH.IProp (InputProps r) (Action pa cs m))
  -> Array (HH.IProp (InputProps r) (Action pa cs m))
setInputProps' parentHandlers props = props <> sharedInputProps <>
  [ HE.onKeyDown $ Just <<< OnKeyDownInput' parentHandlers.onKeyDown
  ]

menuRef :: H.RefLabel
menuRef = H.RefLabel "__nselect_menu"

-- | Use `setMenuProps` so that after ArrowUp/ArrowDown, highlighted item will
-- | still be visible.
setMenuProps
  :: forall pa cs m r
   . Array (HH.IProp r (Action pa cs m))
  -> Array (HH.IProp r (Action pa cs m))
setMenuProps props = props <>
  [ HP.ref menuRef
  ]

type ItemProps r =
  ( onClick :: ME.MouseEvent
  , onMouseEnter :: ME.MouseEvent
  | r
  )

setItemProps
  :: forall pa cs m r
   . Int
  -> Array (HH.IProp (ItemProps r) (Action pa cs m))
  -> Array (HH.IProp (ItemProps r) (Action pa cs m))
setItemProps index props = props <>
  [ HH.attr (HH.AttrName "data-nselect-item") (show index)
  , HE.onClick $ Just <<< const (OnClickItem index)
  , HE.onMouseEnter $ Just <<< const (OnMouseEnterItem index)
  ]

render :: forall pa cs m. InnerState pa cs m -> HTML pa cs m
render state =
  state.props.render $ innerStateToState state

component
  :: forall pa cs m
   . MonadAff m
  => H.Component HH.HTML Query (Props pa cs m) (Message pa) m
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

handleVisibilityChange
  :: forall pa cs m
   . MonadEffect m
  => Boolean -> DSL pa cs m Unit
handleVisibilityChange isOpen = do
  state <- H.modify $ _ { isOpen = isOpen }

  -- Make sure highlighted item is visible when dropdown becomes open.
  when isOpen $
    scrollIntoViewIfNeeded state.highlightedIndex

  H.raise $ VisibilityChanged isOpen

handleHighlightedIndexChange
  :: forall pa cs m
   . MonadEffect m
  => Int
  -> DSL pa cs m Unit
handleHighlightedIndexChange index = do
  H.modify_ $ _ { highlightedIndex = index }
  scrollIntoViewIfNeeded index

scrollIntoViewIfNeeded
  :: forall pa cs m
   . MonadEffect m
  => Int
  -> DSL pa cs m Unit
scrollIntoViewIfNeeded index = do
  H.getHTMLElementRef menuRef >>= traverse_ \menu -> H.liftEffect $ do
    querySelector selector (HTMLElement.toParentNode menu) >>= traverse_ \itemEl -> do
      let
        menuEl = HTMLElement.toElement menu
        item = unsafeCoerce itemEl
      scrollTop <- Element.scrollTop menuEl
      menuHeight <- Element.clientHeight menuEl
      itemOffsetTop <- HTMLElement.offsetTop item
      itemOffsetHeight <- HTMLElement.offsetHeight item

      if scrollTop + menuHeight < itemOffsetTop + itemOffsetHeight
        then Element.setScrollTop (itemOffsetTop + itemOffsetHeight - menuHeight) menuEl
        else if itemOffsetTop < scrollTop
          then Element.setScrollTop itemOffsetTop menuEl
          else pure unit
  where
  selector = QuerySelector $ "[data-nselect-item='" <> show index <> "']"

handleAction
  :: forall pa cs m
   . MonadAff m
  => Action pa cs m
  -> DSL pa cs m Unit
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
    state <- H.get
    let event = KE.toEvent kbEvent
    when state.isOpen $ case KE.key kbEvent of
      "ArrowUp" -> do
        H.liftEffect $ Event.preventDefault event
        s <- H.get
        let nextIndex = max 0 (s.highlightedIndex - 1)
        when (nextIndex /= s.highlightedIndex) $
          handleHighlightedIndexChange nextIndex
      "ArrowDown" -> do
        H.liftEffect $ Event.preventDefault event
        s <- H.get
        let nextIndex = min (s.props.itemCount - 1) (s.highlightedIndex + 1)
        when (nextIndex /= s.highlightedIndex) $
          handleHighlightedIndexChange nextIndex
      "Enter" -> do
        s <- H.get
        when (s.props.itemCount > 0) $
          H.raise $ Selected s.highlightedIndex
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

  Raise pa -> do
    H.raise $ Emit pa


handleQuery
  :: forall pa cs m a
   . MonadAff m
  => Query a
  -> DSL pa cs m (Maybe a)
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

  Highlight index n -> do
    handleHighlightedIndexChange index
    pure $ Just n

  Select n -> do
    H.gets _.highlightedIndex >>= H.raise <<< Selected
    pure $ Just n

  GetState q -> do
    state <- H.get
    pure $ Just $ q $ innerStateToState state

-- | Following are helpers so that you can query from the parent component.
open :: Query Unit
open = Open unit

close :: Query Unit
close = Close unit

focus :: Query Unit
focus = Focus unit

highlight :: Int -> Query Unit
highlight index = Highlight index unit

raise :: forall pa cs m. pa -> Action pa cs m
raise = Raise

select :: Query Unit
select = Select unit

getState :: forall a. (State -> a) -> Query a
getState = GetState
