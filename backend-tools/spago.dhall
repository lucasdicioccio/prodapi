{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "affjax"
  , "console"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "halogen-svg-elems"
  , "ordered-collections"
  , "parseint"
  , "parsing"
  , "psci-support"
  , "tuples"
  , "uri"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
