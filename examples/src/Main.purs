module Main where

import Prelude

import Halogen.CustomElement as CustomElement
import Effect (Effect)
import Example.Autocomplete as ExpAutocomplete
import Example.ComponentInDropdown as ExpComponentInDropdown
import Example.Dropdown as ExpDropdown
import Example.TwoInputs as ExpTwoInputs

main :: Effect Unit
main = do
  CustomElement.define "example-autocomplete" ExpAutocomplete.component
  CustomElement.define "example-dropdown" ExpDropdown.component
  CustomElement.define "example-component-in-dropdown" ExpComponentInDropdown.component
  CustomElement.define "example-two-inputs" ExpTwoInputs.component
