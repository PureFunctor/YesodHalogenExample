{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
import Yesod
import Yesod.Static

staticFiles "static"

newtype App = App
    { getStatic :: Static
    }

mkYesod "App" [parseRoutes|
/ RootR GET
/static StaticR Static getStatic
|]

instance Yesod App

getRootR :: Handler ()
getRootR = sendFile "text/html" "static/index.html"

main :: IO ()
main = do
    static <- static "static"
    warp 3000 $ App static
