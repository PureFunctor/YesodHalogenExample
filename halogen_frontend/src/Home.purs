module Home where

import Prelude

import Data.Array (singleton)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (css)

type State = { current :: { r :: Int, g :: Int, b :: Int }, committed :: { r :: Int, g :: Int, b :: Int } }
data Action = ChangeColor String Int | CommitColor

component :: forall query input output m. H.Component HH.HTML query input output m
component = H.mkComponent {initialState, render, eval: H.mkEval $ H.defaultEval {handleAction = handleAction} }

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

changeColor ::  String -> String -> Maybe Action
changeColor name value = Just $ ChangeColor name (fromMaybe 0 <<< fromString $ value)

colorField :: forall w. String -> HH.HTML w Action
colorField color =
  HH.div [ css "field" ]
  [ HH.label [ css "label" ] [ HH.text color ]
  , HH.div [ css "control" ]
    [ HH.input [ css "input"
               , HP.type_ HP.InputNumber
               , HP.placeholder "0~255"
               , HP.min 0.0
               , HP.max 255.0
               , HE.onValueChange $ changeColor color
               ]
    ]
  ]

colorFieldSubmit :: forall w. HH.HTML w Action
colorFieldSubmit =
 HH.div [ css "field is-grouped is-grouped-right" ]
 [ HH.p [ css "control" ] [ HH.a [ css "button is-primary", HE.onClick \_ -> Just CommitColor ] [ HH.text "Submit" ] ] ]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
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
          , HH.text $ "Current: "
          , HH.br_
          , HH.text $ "Red: " <> show state.current.r
          , HH.br_
          , HH.text $ "Blue: " <> show state.current.b
          , HH.br_
          , HH.text $ "Green: " <> show state.current.g
          , HH.br_
          , HH.br_
          , HH.text $ "Committed: "
          , HH.br_
          , HH.text $ "Red: " <> show state.committed.r
          , HH.br_
          , HH.text $ "Blue: " <> show state.committed.b
          , HH.br_
          , HH.text $ "Green: " <> show state.committed.g
          , HH.br_
          ]
        ]
      ]
    ]
  ]

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  ChangeColor n s -> do
    state <- H.get
    case n of
      "RED" -> do
        H.put ({current: {r: s, g: state.current.g, b: state.current.b}, committed: state.committed})
      "BLUE" -> do
        H.put ({current: {r: state.current.r, g: state.current.g, b: s}, committed: state.committed})
      "GREEN" -> do
        H.put ({current: {r: state.current.r, g: s, b: state.current.b}, committed: state.committed})
      _ -> pure unit
  CommitColor -> do
    state <- H.get
    H.put ({current: state.current, committed: state.current})
