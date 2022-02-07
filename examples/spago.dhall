{ name = "purescript-halogen-nselect"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-custom-element"
  , "halogen-nselect"
  , "prelude"
  , "strings"
  , "web-events"
  , "web-uievents"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs" ]
}
