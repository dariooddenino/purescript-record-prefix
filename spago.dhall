{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "record-prefix"
, dependencies =
  [ "assert"
  , "effect"
  , "heterogeneous"
  , "prelude"
  , "record"
  , "typelevel-eval"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ./packages.dhall
}
