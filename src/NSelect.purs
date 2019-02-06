module NSelect
  ( Props
  , Message
  , Query
  , RenderState
  , HTML
  , Slot
  , raise
  , setToggleProps
  , setContainerProps
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES
import Web.HTML as Web
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as ET

type Props pq m =
  { render :: RenderState -> HTML pq m
  }

type Message pq = pq Unit

data Query pq m a
  = Init a
  | OnReceiveProps (Props pq m) a
  | OnWindowMouseDown a
  | OnMouseDownToggle a
  | OnMouseUpToggle a
  | OnMouseDownContainer a
  | OnMouseUpContainer a
  | Raise (pq Unit) a

type State pq m =
  { props :: Props pq m
  , open :: Boolean
  , clickedInside :: Boolean
  }

initialState :: forall pq m. Props pq m -> State pq m
initialState props =
  { props
  , open: false
  , clickedInside: false
  }

type RenderState =
  { open :: Boolean
  }

stateToRenderState :: forall pq m. State pq m -> RenderState
stateToRenderState { open } =
  { open
  }

type HTML pq m = H.ComponentHTML (Query pq m) () m

type DSL pq m = H.HalogenM (State pq m) (Query pq m) () (Message pq) m

type Slot f m s = H.Slot (Query f m) (Message f) s

type ToggleProps r =
  ( onMouseDown :: ME.MouseEvent
  , onMouseUp :: ME.MouseEvent
  | r
  )

type ContainerProps r =
  ( onMouseDown :: ME.MouseEvent
  , onMouseUp :: ME.MouseEvent
  | r
  )

setToggleProps
  :: forall pq m r
   . Array (HH.IProp (ToggleProps r) (Query pq m Unit))
  -> Array (HH.IProp (ToggleProps r) (Query pq m Unit))
setToggleProps props = props <>
  [ HE.onMouseDown $ HE.input_ OnMouseDownToggle
  , HE.onMouseUp $ HE.input_ OnMouseUpToggle
  ]

setContainerProps
  :: forall pq m r
   . Array (HH.IProp (ContainerProps r) (Query pq m Unit))
  -> Array (HH.IProp (ContainerProps r) (Query pq m Unit))
setContainerProps props = props <>
  [ HE.onMouseDown $ HE.input_ OnMouseDownContainer
  , HE.onMouseUp $ HE.input_ OnMouseUpContainer
  ]

raise :: forall pq m a. pq Unit -> a -> Query pq m a
raise f = Raise f

render :: forall pq m. State pq m -> HTML pq m
render state =
  state.props.render $ stateToRenderState state

component
  :: forall pq m
   . MonadAff m
  => H.Component HH.HTML (Query pq m) (Props pq m) (Message pq) m
component = H.component
  { initialState
  , render
  , eval
  , receiver: HE.input OnReceiveProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where
  eval :: Query pq m ~> DSL pq m
  eval (Init n) = n <$ do
    win <- H.liftEffect Web.window
    H.subscribe $
      ES.eventListenerEventSource ET.mousedown (Window.toEventTarget win)
        (const $ Just $ H.action OnWindowMouseDown)

  eval (OnWindowMouseDown n) = n <$ do
    state <- H.get
    when (not state.clickedInside && state.open) $
      H.modify_ $ _ { open = false }

  eval (OnReceiveProps props n) = n <$ do
    H.modify_ $ _ { props = props }

  eval (Raise pq n) = n <$ do
    H.raise pq

  eval (OnMouseDownToggle n) = n <$ do
    H.modify_ \s -> s
      { open = not s.open
      , clickedInside = true
      }

  eval (OnMouseUpToggle n) = n <$ do
    H.modify_ $ _
      { clickedInside = false
      }

  eval (OnMouseDownContainer n) = n <$ do
    H.modify_ $ _ { clickedInside = true }

  eval (OnMouseUpContainer n) = n <$ do
    H.modify_ $ _ { clickedInside = false }
