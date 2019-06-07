module NSelect.Component where

import Prelude

import Data.Array as Array
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

data Query a
  = Open a
  | Close a
  | Focus a
  | Highlight Int a
  | Select a
  | GetState (State -> a)

type InnerState r =
  { select :: SelectState
  , getItemCount :: {|r} -> Int
  | r
  }

type SelectState =
  { isOpen :: Boolean
  , clickedInside :: Boolean
  , highlightedIndex :: Int
  }

type State =
  { isOpen :: Boolean
  , highlightedIndex :: Int
  }

initialState :: SelectState
initialState =
  { isOpen: false
  , clickedInside: false
  , highlightedIndex: 0
  }

selectStateToState :: SelectState -> State
selectStateToState { isOpen, highlightedIndex } =
  { isOpen, highlightedIndex }

getExtraState :: forall r. InnerState r -> {|r}
getExtraState = unsafeCoerce

data Message
  = Selected Int
  | InputValueChanged String
  | VisibilityChanged Boolean

data Action a
  = Init
  | OnWindowMouseDown
  | OnMouseDownRoot
  | OnMouseUpRoot
  | OnMouseDownToggle
  | ExtraAction a
  | OnFocusInput
  | OnKeyDownInput KE.KeyboardEvent
  | OnKeyDownInput' (KeyDownHandler a) KE.KeyboardEvent
  | OnClickItem Int
  | OnMouseEnterItem Int
  | OnValueInput String

type DSL state action slot o m =
  H.HalogenM (InnerState state) (Action action) slot o m

handleVisibilityChange :: forall s a sl o m. Boolean -> DSL s a sl o m Unit
handleVisibilityChange isOpen = do
  H.modify_ $ _ { select { isOpen = isOpen } }

handleHighlightedIndexChange
  :: forall s a sl o m
   . MonadEffect m
  => Int
  -> DSL s a sl o m Unit
handleHighlightedIndexChange index = do
  H.modify_ $ _ { select { highlightedIndex = index } }
  scrollIntoViewIfNeeded index

scrollIntoViewIfNeeded
  :: forall state act slot o m
   . MonadEffect m
  => Int
  -> DSL state act slot o m Unit
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

type KeyDownHandler pa = KE.KeyboardEvent -> (Action pa)

-- | setInputProps' does everything setInputProps does, but also pass the
-- | keyboardEvent back to the parent component, so the parent can handle more
-- | key bindings like Tab.
setInputProps'
  :: forall pa r
   . { onKeyDown :: KeyDownHandler pa }
  -> Array (HH.IProp (InputProps r) (Action pa))
  -> Array (HH.IProp (InputProps r) (Action pa))
setInputProps' parentHandlers props = props <> sharedInputProps <>
  [ HE.onKeyDown $ Just <<< OnKeyDownInput' parentHandlers.onKeyDown
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

type Spec s a cs i o m =
  { initialize :: Maybe (Action a)
  , receive :: i -> Maybe (Action a)
  , handleAction :: a -> DSL s a cs o m Unit
  , handleMessage :: Message -> DSL s a cs o m Unit
  }

defaultEval :: forall s a cs i o m. Spec s a cs i o m
defaultEval =
  { initialize: Nothing
  , receive: const Nothing
  , handleAction: const $ pure unit
  , handleMessage: const $ pure unit
  }

mkEval
  :: forall s a cs i o m n
   . MonadAff m
  => Spec s a cs i o m
  -> H.HalogenQ Query (Action a) i n
  -> DSL s a cs o m n
mkEval spec =
  H.mkEval $ H.defaultEval
    { initialize = Just Init
    , receive = spec.receive
    , handleAction = handleAction spec
    , handleQuery = handleQuery spec
    }

handleAction
  :: forall s a cs i o m
   . MonadAff m
  => Spec s a cs i o m
  -> Action a -> DSL s a cs o m Unit
handleAction spec = case _ of
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
        s <- H.get
        let nextIndex = max 0 (s.select.highlightedIndex - 1)
        when (nextIndex /= s.select.highlightedIndex) $
          handleHighlightedIndexChange nextIndex
      "ArrowDown" -> do
        H.liftEffect $ Event.preventDefault event
        s <- H.get
        let nextIndex = min (s.getItemCount (getExtraState s) - 1) (s.select.highlightedIndex + 1)
        when (nextIndex /= s.select.highlightedIndex) $
          handleHighlightedIndexChange nextIndex
      "Enter" -> H.gets _.select.highlightedIndex >>= spec.handleMessage <<< Selected
      _ -> pure unit

  OnKeyDownInput' parentOnKeyDown kbEvent -> do
    handleAction spec (OnKeyDownInput kbEvent)
    handleAction spec $ parentOnKeyDown kbEvent

  OnClickItem index -> do
    spec.handleMessage $ Selected index

  OnMouseEnterItem index -> do
    H.modify_ $ _
      { select
          { highlightedIndex = index }
      }

  OnValueInput value -> do
    handleHighlightedIndexChange 0
    spec.handleMessage $ InputValueChanged value

  ExtraAction action -> do
    spec.handleAction action

handleQuery
  :: forall s a cs i o m n
   . MonadAff m
  => Spec s a cs i o m
  -> Query n -> DSL s a cs o m (Maybe n)
handleQuery spec = case _ of
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
    H.gets _.select.highlightedIndex >>= spec.handleMessage <<< Selected
    pure $ Just n

  GetState q -> do
    state <- H.get
    pure $ Just $ q $ selectStateToState state.select
