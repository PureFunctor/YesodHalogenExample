module Shared.HTML where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Shared.Utils (css)


navbar :: forall w i. HH.HTML w i
navbar =
  HH.nav [ css "navbar is-dark" ]
    [ HH.div [ css "navbar-start" ]
      [ HH.a [ css "navbar-item" , HP.href "/#/" ] [ HH.text "Home" ]
      , HH.a [ css "navbar-item" , HP.href "/#/about" ] [ HH.text "About" ]
      ]
    ]
