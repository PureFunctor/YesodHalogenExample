module Shared.Utils where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- Utility function for adding a CSS class
css :: forall r i. String -> HP.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName
