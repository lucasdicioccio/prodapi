module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..))
import Data.List as List
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

import Data.Foldable (fold)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Map (Map)
import Data.Map as Map
import Parsing.Prometheus (promDoc, PromDoc, Line(..), Labels, LabelPair, pairName, pairValue, MetricName, MetricValue)

type PromData =
  { metrics :: Map (Tuple MetricName Labels) MetricValue
  , helps :: Map MetricName String
  }

fromPromDoc :: PromDoc -> PromData
fromPromDoc metrics =
  { metrics: Map.fromFoldable $ List.catMaybes $ map toMetric metrics
  , helps: Map.fromFoldable $ List.catMaybes $ map toHelp metrics
  }
  where
    toMetric (MetricLine n lbls val _) = let key = Tuple n lbls in Just $ Tuple key val
    toMetric _                         = Nothing
    toHelp (HelpLine n s)              = Just (Tuple n s)
    toHelp _                           = Nothing

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

type State =
  { statusResult :: Maybe String
  , metricsResult :: Maybe String
  , metricsHistory :: List PromData
  }

data Action
  = MakeStatusRequest MouseEvent
  | MakeMetricsRequest MouseEvent

component
  :: forall query input output m. MonadAff m
  => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { statusResult: Nothing, metricsResult: Nothing, metricsHistory: Nil }

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
    , renderPromHistory
        $ st.metricsHistory
    ]

renderPromHistory :: forall m. List PromData -> H.ComponentHTML Action () m
renderPromHistory history =
  HH.table_
    $ map (\key -> renderPromLine key)
    $ Set.toUnfoldable
    $ allKeys

  where
    allKeys :: Set (Tuple MetricName Labels)
    allKeys = fold (map (Map.keys <<< _.metrics) history)

    renderPromLine key =
      let n    = Tuple.fst key
          lbls = Tuple.snd key
          timeseries = map (Map.lookup key <<< _.metrics) history
      in
      HH.tr_ [ HH.td_ [ HH.text n ]
             , HH.td_ [ renderLabels lbls ]
             , HH.td_ [ renderValues timeseries ]
             ]

    renderValues xs =
      HH.div_
        $ List.toUnfoldable
        $ map (\v -> HH.span_ [ HH.text $ v <> " "])
        $ map (maybe "NA" show) xs

renderLabels :: forall m. Labels -> H.ComponentHTML Action () m
renderLabels labels =
  HH.div_
    $ List.toUnfoldable
    $ map renderLabelPair labels

renderLabelPair :: forall m. LabelPair -> H.ComponentHTML Action () m
renderLabelPair pair =
  HH.span_
    [ HH.strong_ [ HH.text $ pairName pair ]
    , HH.text ": "
    , HH.text $ pairValue pair
    ]

renderValue :: forall m. MetricValue -> H.ComponentHTML Action () m
renderValue val =
  HH.strong_ [ HH.text $ show val ]

renderGetStatus :: forall m. State -> H.ComponentHTML Action () m
renderGetStatus st =
  HH.button
    [ HP.type_ HP.ButtonSubmit
    , HE.onClick \ev -> Just (MakeStatusRequest ev)
    ]
    [ HH.text "Status" ]

renderGetMetrics :: forall m. State -> H.ComponentHTML Action () m
renderGetMetrics st =
  HH.button
    [ HP.type_ HP.ButtonSubmit
    , HE.onClick \ev -> Just (MakeMetricsRequest ev)
    ]
    [ HH.text "Metrics" ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of

  MakeStatusRequest event -> do
    response <- H.liftAff $ AX.get AXRF.string ("/status")
    H.modify_ _ { statusResult = map _.body (hush response) }

  MakeMetricsRequest event -> do
    response <- H.liftAff $ AX.get AXRF.string ("/metrics")
    let prom = hush response >>= parseBody
    let promlist = maybe Nil (\dat -> List.singleton $ fromPromDoc dat) prom
    H.modify_ \state -> state { metricsResult = map _.body (hush response)
                              , metricsHistory = promlist <> state.metricsHistory
                              }

parseBody :: forall t. { body :: String | t } -> Maybe PromDoc
parseBody = hush <<< (flip runParser) promDoc <<< _.body

