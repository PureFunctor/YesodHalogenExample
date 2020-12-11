module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Pages.Home as Home

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Home.component unit body
