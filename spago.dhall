{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-kernel"
, dependencies =
  [ "convertable-options"
  , "datetime"
  , "effect"
  , "either"
  , "erl-atom"
  , "erl-binary"
  , "erl-lists"
  , "erl-process"
  , "erl-tuples"
  , "erl-untagged"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "integers"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "record"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
