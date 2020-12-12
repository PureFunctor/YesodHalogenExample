module Router where

import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Pages.About as AboutPage
import Pages.Home as HomePage
import Prelude (class Eq, Unit, Void, absurd, bind, pure, unit, ($), (<$>), (<<<), discard)
import Routing.Duplex as RD
import Routing.Duplex.Generic (noArgs)
import Routing.Duplex.Generic as RD
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (getHash, setHash)

data Page = Home | About

derive instance genericPage :: Generic Page _
derive instance eqPage :: Eq Page

pageCodec :: RD.RouteDuplex' Page
pageCodec = RD.root $ RD.sum
  { "Home": noArgs
  , "About": "about" / noArgs
  }

type State = { currentPage :: Page }
data Action = Initialize
data Query a = Navigate Page a
type ChildSlots =
  ( home :: H.Slot Query Void Unit
  , about :: H.Slot Query Void Unit
  )


component :: forall input output m. MonadAff m => H.Component HH.HTML Query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , initialize = Just Initialize
    }
  }
  where
  initialState :: input -> State
  initialState _ = { currentPage : Home }

  render { currentPage } = case currentPage of
    Home -> HH.slot (SProxy :: _ "home") unit HomePage.component unit absurd
    About -> HH.slot (SProxy :: _ "about") unit AboutPage.component unit absurd

  handleAction :: Action -> H.HalogenM State Action ChildSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse pageCodec) <$> liftEffect getHash
      liftEffect <<< setHash <<< RD.print pageCodec <<< fromMaybe Home $ initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots output m (Maybe a)
  handleQuery = case _ of
    Navigate page a -> do
      { currentPage } <- H.get
      H.put { currentPage: page }
      pure (Just a)
