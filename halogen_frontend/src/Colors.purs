module Colors where

import Prelude

import Data.Array (singleton)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Console (log)
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


mkAncestorTile :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkAncestorTile = HH.div [ css "tile is-ancestor" ]


mkParentTile :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkParentTile = HH.div [ css "tile is-parent" ]


mkChildTile :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkChildTile = HH.div [ css "tile is-child notification is-light" ]


mkTitle :: forall w i. String -> HH.HTML w i
mkTitle = HH.p [ css "title has-text-dark" ] <<< singleton <<< HH.text


type RGBValues = { r :: Int , g :: Int , b :: Int }
type FormsState = { current :: RGBValues }
data FormsAction = FormUpdateColor RGB Int | FormSubmission
type FormsOutput = RGBValues


formsComponent :: forall query input m. H.Component HH.HTML query input FormsOutput m
formsComponent =
  H.mkComponent
  { initialState : formsInitialState
  , render : formsRender
  , eval : H.mkEval $ H.defaultEval
    { handleAction = formsHandleAction
    }
  }


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
      , HE.onValueChange (Just <<< FormUpdateColor color <<< fromMaybe 0 <<< fromString)
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
    , formsSubmit
    ]
  ]


formsHandleAction :: forall m. FormsAction -> H.HalogenM FormsState FormsAction () FormsOutput m Unit
formsHandleAction = case _ of
  FormUpdateColor color value -> do
    { current } <- H.get
    case color of
      Red -> H.put { current : current { r = value } }
      Green -> H.put { current : current { g = value } }
      Blue -> H.put { current : current { b = value } }
  FormSubmission -> do
    { current } <- H.get
    H.raise current


type ParentState = Unit
data ParentAction = HandleSubmission FormsOutput

type ParentSlots = ( forms :: forall q. H.Slot q FormsOutput Unit )

_forms = SProxy :: SProxy "forms"


parentComponent :: forall query input output m. (MonadEffect m) => H.Component HH.HTML query input output m
parentComponent =
  H.mkComponent
  { initialState : (\_ -> unit)
  , render : parentRender
  , eval: H.mkEval $ H.defaultEval
    { handleAction = parentHandleAction
    }
  }


parentRender :: forall m. (MonadEffect m) => ParentState -> H.ComponentHTML ParentAction ParentSlots m
parentRender _ =
  HH.div [ css "hero is-dark is-fullheight" ]
  [ HH.div [ css "hero-body" ]
    [ mkAncestorTile
      [ HH.slot _forms unit formsComponent { current: { r : 0 , g : 0 , b : 0 } } (Just <<< HandleSubmission)
      ]
    ]
  ]


parentHandleAction :: forall output m. (MonadEffect m) => ParentAction -> H.HalogenM ParentState ParentAction ParentSlots output m Unit
parentHandleAction = case _ of
  HandleSubmission current ->
    H.liftEffect $ log $ show current
