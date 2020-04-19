{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "zeromq"
, dependencies =
  [ "aff"
  , "argonaut"
  , "console"
  , "effect"
  , "foreign"
  , "generics-rep"
  , "node-buffer"
  , "nullable"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
