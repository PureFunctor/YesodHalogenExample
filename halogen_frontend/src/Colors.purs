module Colors where

import Prelude

import Halogen as H

data RGB = Red | Green | Blue

instance showRGB :: Show RGB where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"


type RGBValues = { r :: Int , g :: Int , b :: Int }
type FormsState = { current :: RGBValues }
data FormsAction = FormSubmission


formsInitialState :: forall input. input -> FormsState
formsInitialState _ = { current : { r : 0 , g : 0  , b : 0 } }


formsHandleAction :: forall m. FormsAction -> H.HalogenM FormsState FormsAction () RGBValues m Unit
formsHandleAction = case _ of
  FormSubmission -> do
    { current } <- H.get
    H.raise current
