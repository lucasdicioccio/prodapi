module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush, Either(..))
import Data.Maybe (Maybe(..))
import Data.List (filter, toUnfoldable)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.UIEvent.MouseEvent (MouseEvent)
import Text.Parsing.Parser (runParser)

import Parsing.Prometheus (promDoc, PromDoc, Line(..))

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

type State =
  { loading :: Boolean
  , username :: String
  , statusResult :: Maybe String
  , metricsResult :: Maybe String
  }

data Action
  = MakeStatusRequest MouseEvent
  | MakeMetricsRequest MouseEvent

component :: forall query input output m. MonadAff m => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { loading: false, username: "", statusResult: Nothing, metricsResult: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div_
    [ renderGetStatus st
    , renderGetMetrics st
    , HH.div_
        case st.statusResult of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Status:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ] ]
            ]
    , HH.div_
        case st.metricsResult of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Metrics:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ]
                ]
            , case runParser res promDoc of
                Left err -> HH.code_ [ HH.text $ show err ]
                Right pdoc -> HH.div_ [ renderPromDoc pdoc , HH.text $ show pdoc ]
            ]
    ]

renderPromDoc :: forall m. PromDoc -> H.ComponentHTML Action () m
renderPromDoc metrics =
  HH.ul_
    $ toUnfoldable
    $ map renderPromLine
    $ filter isMetric metrics

  where
    isMetric (MetricLine _ _ _ _) = true
    isMetric _                    = false

renderPromLine :: forall m. Line -> H.ComponentHTML Action () m
renderPromLine = case _ of
  MetricLine n lbls val _ -> HH.li_ [ HH.p_ [ HH.text n ]
                                    , HH.p_ [ HH.text $ show lbls ]
                                    , HH.p_ [ HH.text $ show val ]
                                    ]
  _                       -> HH.div_ []

renderGetStatus :: forall m. State -> H.ComponentHTML Action () m
renderGetStatus st =
  HH.button
    [ HP.disabled st.loading
    , HP.type_ HP.ButtonSubmit
    , HE.onClick \ev -> Just (MakeStatusRequest ev)
    ]
    [ HH.text "Status" ]

renderGetMetrics :: forall m. State -> H.ComponentHTML Action () m
renderGetMetrics st =
  HH.button
    [ HP.disabled st.loading
    , HP.type_ HP.ButtonSubmit
    , HE.onClick \ev -> Just (MakeMetricsRequest ev)
    ]
    [ HH.text "Metrics" ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of

  MakeStatusRequest event -> do
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.string ("/status")
    H.modify_ _ { loading = false, statusResult = map _.body (hush response) }

  MakeMetricsRequest event -> do
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.string ("/metrics")
    H.modify_ _ { loading = false, metricsResult = map _.body (hush response) }

