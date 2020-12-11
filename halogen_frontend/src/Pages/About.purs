module Pages.About where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Shared.Utils (css)


type State = Unit


component :: forall query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }
  where
  initialState :: input -> Unit
  initialState _ = unit

  render :: forall action. State -> H.ComponentHTML action () m
  render _ =
    HH.div [ css "hero is-dark is-fullheight" ]
    [ HH.div [ css "hero-body" ]
      [ HH.h1_ [ HH.text "About Page" ]
      ]
    ]
