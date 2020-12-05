{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "YesodHalogenExample"
, dependencies = [ "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "halogen_frontend/src/**/*.purs" ]
}
