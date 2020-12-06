module Home where

import Prelude

import Data.Array (singleton)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Utils (css)

type State = { current :: { r :: Int, g :: Int, b :: Int }, committed :: { r :: Int, g :: Int, b :: Int } }
data Action = ChangeColor String Int | CommitColor

component :: forall query input output m. H.Component HH.HTML query input output m
component = H.mkComponent {initialState, render, eval: H.mkEval $ H.defaultEval}

initialState :: forall input. input -> State
initialState _ = { current : { r: 0, g: 0, b: 0 }, committed : { r: 0, g: 0, b: 0 } }

mkParentTile :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkParentTile = HH.div [ css "tile is-parent" ]

mkChildTile :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkChildTile = HH.div [ css "tile is-child notification is-light" ]

mkTitle :: forall w i. HH.HTML w i -> HH.HTML w i
mkTitle = HH.p [ css "title has-text-dark" ] <<< singleton

mkSubtitle :: forall w i. HH.HTML w i-> HH.HTML w i
mkSubtitle = HH.p [ css "subtitle has-text-dark" ] <<< singleton

colorField :: forall w i. String -> HH.HTML w i
colorField color =
  HH.div [ css "field" ]
  [ HH.label [ css "label" ] [ HH.text color ]
  , HH.div [ css "control" ]
    [ HH.input [ css "input", HP.type_ HP.InputNumber, HP.placeholder "0~255", HP.min 0.0, HP.max 255.0 ] ]
  ]

colorFieldSubmit :: forall w i. HH.HTML w i
colorFieldSubmit =
 HH.div [ css "field is-grouped is-grouped-right" ]
 [ HH.p [ css "control" ] [ HH.a [ css "button is-primary" ] [ HH.text "Submit" ] ] ]

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ css "hero is-dark is-fullheight" ]
  [ HH.div [ css "hero-body" ]
    [ HH.div [ css "tile is-ancestor" ]
      [ mkParentTile
        [ mkChildTile
          [ mkTitle $ HH.text "Colors"
          , colorField "RED"
          , colorField "BLUE"
          , colorField "GREEN"
          , colorFieldSubmit
          ]
        ]
      , mkParentTile
        [ mkChildTile
          [ mkTitle $ HH.text "Output"
          , mkSubtitle $ HH.text "All told, a monad in X is just a monoid in the category of endofunctors of X, with product × replaced by composition of endofunctors and unit set by the identity endofunctor."
          ]
        ]
      ]
    ]
  ]
