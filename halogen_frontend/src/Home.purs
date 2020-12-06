module Home where

import Prelude

import Halogen as H
import Halogen.HTML as HH

type State = Unit
type Action = Unit

component :: forall query input output m. H.Component HH.HTML query input output m
component = H.mkComponent {initialState, render, eval: H.mkEval $ H.defaultEval}

initialState :: forall input. input -> State
initialState _ = unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ = HH.div_ [ HH.h1_ [ HH.text "Hello, World"] ]
