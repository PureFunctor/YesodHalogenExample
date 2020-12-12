{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "YesodHalogenExample"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  ]
, packages = ./packages.dhall
, sources = [ "halogen_frontend/src/**/*.purs" ]
}
