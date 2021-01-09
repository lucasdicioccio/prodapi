module Main where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Halogen.Aff (awaitBody, selectElement, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Effect.Aff (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Monitor (render, handleAction, State, Action(..))
import History (emptyHistory)
import HistoryPacked as PH
import Web.DOM.ParentNode (QuerySelector(..))

foreign import tabUrl :: (String -> Effect Unit) -> Effect Unit

main :: Effect Unit
main = do
  ref <- Ref.new ""
  tabUrl (\url -> Ref.write url ref)
  runHalogenAff do
    body <- awaitBody
    elem <- selectElement (QuerySelector "#metrics")
    let tgt = fromMaybe body elem
    runUI (component ref) unit tgt

component
  :: forall query input output m. MonadAff m
  => Ref.Ref String
  -> H.Component HH.HTML query input output m
component url =
  H.mkComponent
    { initialState: initialState url
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ StartPolling $ Milliseconds 1000.0
        }
    }

initialState :: forall i. Ref.Ref String -> i -> State
initialState urlRef _ =
  { metricsResult: Nothing
  , nsamples: 100
  , nextrasamples: 30
  , displayedCharts: Nil
  , polling : Nothing
  , pollingPeriod : Milliseconds 1000.0
  , metricsRequest : defaultMakeRequest
  , history : emptyHistory
  , packedHistory : PH.emptyHistory
  , historyData : Map.empty
  }
  where
    defaultMakeRequest =  do
      baseUrl <- liftEffect $ Ref.read urlRef
      pure AX.defaultRequest
              { url = baseUrl <> "/metrics"
              , responseFormat = AXRF.string 
              , timeout = Just $ Milliseconds 250.0 
              }
