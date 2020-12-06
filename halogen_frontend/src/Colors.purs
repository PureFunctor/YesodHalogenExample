module Colors where

import Prelude

import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (css)

data RGB = Red | Green | Blue

instance showRGB :: Show RGB where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"


mkParentTile :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkParentTile = HH.div [ css "tile is-parent" ]


mkChildTile :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkChildTile = HH.div [ css "tile is-child notification is-light" ]


mkTitle :: forall w i. String -> HH.HTML w i
mkTitle = HH.p [ css "title has-text-dark" ] <<< singleton <<< HH.text


type RGBValues = { r :: Int , g :: Int , b :: Int }
type FormsState = { current :: RGBValues }
data FormsAction = FormSubmission
type FormsOutput = RGBValues


formsComponent :: forall query input m. H.Component HH.HTML query input FormsOutput m
formsComponent = H.mkComponent { initialState: formsInitialState , render: formsRender , eval: H.mkEval $ H.defaultEval { handleAction = formsHandleAction } }


formsInitialState :: forall input. input -> FormsState
formsInitialState _ = { current : { r : 0 , g : 0  , b : 0 } }


formsField :: forall w. RGB -> HH.HTML w FormsAction
formsField color =
  HH.div [ css "field" ]
  [ HH.label [ css "label" ]
    [ HH.text <<< show $ color
    ]
  , HH.div [ css "control" ]
    [ HH.input
      [ css "input"
      , HP.type_ HP.InputNumber
      , HP.placeholder "0~255"
      , HP.min 0.0
      , HP.max 255.0
      ]
    ]
  ]


formsSubmit :: forall w. HH.HTML w FormsAction
formsSubmit =
  HH.div [ css "field is-grouped is-grouped-right" ]
  [ HH.p [ css "control" ]
    [ HH.a [ css "button is-primary" , HE.onClick \_ -> Just FormSubmission ]
      [ HH.text "Submit"
      ]
    ]
  ]


formsRender :: forall m. FormsState -> H.ComponentHTML FormsAction () m
formsRender _ =
  mkParentTile
  [ mkChildTile
    [ mkTitle "Colors"
    , formsField Red
    , formsField Green
    , formsField Blue
    ]
  ]


formsHandleAction :: forall m. FormsAction -> H.HalogenM FormsState FormsAction () FormsOutput m Unit
formsHandleAction = case _ of
  FormSubmission -> do
    { current } <- H.get
    H.raise current
