module NSelect.Component where

import Prelude

import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Component as HC
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

type State item r =
  { select :: SelectStateRow
  , items :: Array item
  | r
  }

type SelectStateRow =
  { isOpen :: Boolean
  , clickedInside :: Boolean
  , highlightedIndex :: Int
  }

initialState :: SelectStateRow
initialState =
  { isOpen: false
  , clickedInside: false
  , highlightedIndex: 0
  }

data Message
  = Selected Int
  | InputValueChanged String

data Action a
  = Init
  | OnWindowMouseDown
  | OnMouseDownRoot
  | OnMouseUpRoot
  | OnMouseDownToggle
  | ExtraAction a
  | OnFocusInput
  | OnKeyDownInput KE.KeyboardEvent
  | OnClickItem Int
  | OnMouseEnterItem Int
  | OnValueInput String

defaultEval
  :: forall x q s a sl i o m
   . HC.EvalSpec (State x s) q (Action a) sl i o m
defaultEval = H.defaultEval
  { initialize = Just Init
  }

type DSL item state action slot o m =
  H.HalogenM (State item state) (Action action) slot o m

handleVisibilityChange :: forall x s a sl o m. Boolean -> DSL x s a sl o m Unit
handleVisibilityChange isOpen = do
  H.modify_ $ _ { select { isOpen = isOpen } }

handleHighlightedIndexChange
  :: forall x s a sl o m
   . MonadEffect m
  => Int
  -> DSL x s a sl o m Unit
handleHighlightedIndexChange index = do
  H.modify_ $ _ { select { highlightedIndex = index } }
  scrollIntoViewIfNeeded index

scrollIntoViewIfNeeded
  :: forall item state act slot o m
   . MonadEffect m
  => Int
  -> DSL item state act slot o m Unit
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
  :: forall x s a sl o m
   . MonadAff m
  => (a -> DSL x s a sl o m Unit)
  -> (Message -> DSL x s a sl o m Unit)
  -> Action a
  -> DSL x s a sl o m Unit
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
        s <- H.get
        let nextIndex = max 0 (s.select.highlightedIndex - 1)
        when (nextIndex /= s.select.highlightedIndex) $
          handleHighlightedIndexChange nextIndex
      "ArrowDown" -> do
        H.liftEffect $ Event.preventDefault event
        s <- H.get
        let nextIndex = min (Array.length s.items - 1) (s.select.highlightedIndex + 1)
        when (nextIndex /= s.select.highlightedIndex) $
          handleHighlightedIndexChange nextIndex
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
    handleHighlightedIndexChange 0
    handleMessage $ InputValueChanged value

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
