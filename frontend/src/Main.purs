module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Data.Maybe (Maybe(..))
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff)
import Routes

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = { enabled :: Boolean, person :: Maybe Person }

data Action
  = Toggle
  | GetPerson

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false, person: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = if state.enabled then "On" else "Off"
  in
    HH.button
      [ HP.title label
      , HE.onClick \_ -> Just GetPerson
      ]
      [ HH.text label ]

handleAction ∷ forall o m. MonadAff m => Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> H.modify_ \st -> st { enabled = not st.enabled }
  GetPerson -> do
    { person: person } <- H.get
    liftAff
      $ case person of
          _ -> do
            _response <- Aff.attempt $ M.fetch nodeFetch (M.URL "http://127.0.0.1:8081/person") M.defaultFetchOptions
            case _response of
              Left err -> liftEffect $ log $ show err
              Right response -> do
                txt <- M.text response
                liftEffect $ log txt
