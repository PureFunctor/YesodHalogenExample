module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Router as Router

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Router.component unit body
