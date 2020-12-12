module Router where

import Prelude

import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Pages.About as AboutPage
import Pages.Home as HomePage

data Page = Home | About
type State = { currentPage :: Page }

component :: forall query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }
  where
  initialState :: input -> State
  initialState _ = { currentPage : Home }

  render { currentPage } = case currentPage of
    Home -> HH.slot (SProxy :: _ "home") unit HomePage.component unit absurd
    About -> HH.slot (SProxy :: _ "about") unit AboutPage.component unit absurd
