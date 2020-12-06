module Home where

import Prelude

import Data.Array (singleton)

import Halogen as H
import Halogen.HTML as HH

import Utils (css)

type State = Unit
type Action = Unit

component :: forall query input output m. H.Component HH.HTML query input output m
component = H.mkComponent {initialState, render, eval: H.mkEval $ H.defaultEval}

initialState :: forall input. input -> State
initialState _ = unit

mkParentTile :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkParentTile = HH.div [ css "tile is-parent" ]

mkChildTile :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkChildTile = HH.div [ css "tile is-child notification is-light" ]

mkTitle :: forall w i. HH.HTML w i -> HH.HTML w i
mkTitle = HH.p [ css "title has-text-dark" ] <<< singleton

mkSubtitle :: forall w i. HH.HTML w i-> HH.HTML w i
mkSubtitle = HH.p [ css "subtitle has-text-dark" ] <<< singleton

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ css "hero is-dark is-fullheight" ]
  [ HH.div [ css "hero-body" ]
    [ HH.div [ css "tile is-ancestor" ]
      [ mkParentTile
        [ mkChildTile
          [ mkTitle $ HH.text "Input Here"
          , mkSubtitle $ HH.text "All told, a monad in X is just a monoid in the category of endofunctors of X, with product × replaced by composition of endofunctors and unit set by the identity endofunctor."
          ]
        ]
      , mkParentTile
        [ mkChildTile
          [ mkTitle $ HH.text "Output Here"
          , mkSubtitle $ HH.text "All told, a monad in X is just a monoid in the category of endofunctors of X, with product × replaced by composition of endofunctors and unit set by the identity endofunctor."
          ]
        ]
      ]
    ]
  ]
