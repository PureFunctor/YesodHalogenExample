module Pages.Home where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Shared.HTML (navbar)
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
    HH.div_
    [ navbar
    , HH.section [ css "hero is-primary has-text-centered" ]
      [ HH.div [ css "hero-body" ]
        [ HH.h1 [ css "is-size-1" ]
          [ HH.text "Home" ]
        ]
      ]
    ]
