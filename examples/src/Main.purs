module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Dropdown as ExpDropdown
import Example.Autocomplete as ExpAutocomplete
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "Dropdown" $ proxy ExpDropdown.component
  , Tuple "Autocomplete" $ proxy ExpAutocomplete.component
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook
    { stories
    , logo: Nothing
    }
