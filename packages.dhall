let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220127/packages.dhall sha256:8ccbd53dbc7dbfd92a9cba9cca7a8bf36cb120a0a3e21106bf19a16d3ad6863e

in  upstream
  with halogen-custom-element =
      mkPackage
        [ "aff", "effect", "halogen", "prelude", "psci-support", "web-html" ]
        "https://github.com/input-output-hk/purescript-halogen-custom-element.git"
        "05dd1f21a00f0e21824a39b57737e41ec4ee6c19"
