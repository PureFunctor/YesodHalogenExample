module Colors where

import Data.Show (class Show)

data RGB = Red | Green | Blue

instance showRGB :: Show RGB where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"
