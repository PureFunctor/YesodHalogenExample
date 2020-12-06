module Colors where

import Data.Show (class Show)

data RGB = Red | Green | Blue

instance showRGB :: Show RGB where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"


type FormsState = { current :: { r :: Int , g :: Int , b :: Int } }


formsInitialState :: forall input. input -> FormsState
formsInitialState _ = { current : { r : 0 , g : 0  , b : 0 } }
