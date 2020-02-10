let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200127/packages.dhall sha256:06a623f48c49ea1c7675fdf47f81ddb02ae274558e29f511efae1df99ea92fb8

let overrides = {=}

let additions =
      { halogen-custom-element =
          { dependencies = [ "prelude", "halogen", "effect", "aff" ]
          , repo =
              "https://github.com/nonbili/purescript-halogen-custom-element.git"
          , version = "e4f5a39a865df775a3f047e8f6df4aee53431d6d"
          }
      }

in  upstream // overrides // additions
