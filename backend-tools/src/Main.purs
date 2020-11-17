module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Control.Monad.Rec.Class (forever)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.List (List(..))
import Data.List as List
import Effect (Effect)
import Effect.Exception (error)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as EventSource
import Halogen.VDom.Driver (runUI)
import Web.UIEvent.MouseEvent (MouseEvent)
import Text.Parsing.Parser (runParser)

import Data.Foldable (fold, maximum)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Map (Map)
import Data.Map as Map
import Parsing.Prometheus (promDoc, PromDoc, Line(..), Labels, LabelPair, pairName, pairValue, MetricName, MetricValue)
import Charting.Charts (ChartSpec(..), specIndex, ChartDisplayMode(..), cycleDisplayMode, specKeys)
import Charting.SparkLine (renderSparkline)
import Charting.TimeSeries (renderChartTimeseries, renderChartDiffTimeseries, renderChartSmoothTimeseries, renderMultiChartTimeseries)

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

showDisplayMode :: ChartDisplayMode -> String
showDisplayMode = case _ of
  Samples -> "raw"
  DiffSamples -> "delta"
  Smooth -> "smooth"

type State =
  { statusResult :: Maybe String
  , metricsResult :: Maybe String
  , nsamples :: Int
  , nextrasamples :: Int
  , metricsHistory :: List PromData
  , displayedCharts :: List ChartSpec
  , polling :: Maybe H.SubscriptionId
  }

data Action
  = MakeStatusRequest MouseEvent
  | MakeMetricsRequest
  | ZoomMetric MetricName Labels
  | UnZoomMetric Int
  | MergeMetric MetricName Labels
  | CycleChartSpec Int
  | StartPolling
  | StopPolling

component
  :: forall query input output m. MonadAff m
  => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just StartPolling
        }
    }

initialState :: forall i. i -> State
initialState _ =
  { statusResult: Nothing
  , metricsResult: Nothing
  , nsamples: 100
  , nextrasamples: 30
  , metricsHistory: Nil
  , displayedCharts: Nil
  , polling : Nothing
  }

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
        st.metricsHistory
        st.displayedCharts
    ]

renderPromHistory
  :: forall m
  .  List PromData
  -> List ChartSpec
  -> H.ComponentHTML Action () m
renderPromHistory history chartspecs =
    HH.div_
      [ renderZoomedCharts
      , renderPromTable
      ]
  where
    allKeys :: Set (Tuple MetricName Labels)
    allKeys = fold (map (Map.keys <<< _.metrics) history)

    renderZoomedCharts =
      HH.ul_
        $ map (\spec -> renderChart spec)
        $ List.toUnfoldable
        $ chartspecs

    renderPromTable =
      HH.table_
        $ map (\key -> renderPromLine key)
        $ Set.toUnfoldable
        $ allKeys

    renderChart (SingleTimeSeries idx k n lbls) =
      let key  = Tuple n lbls
          timeseries = map (Map.lookup key <<< _.metrics) history
      in
      HH.div_
        [ HH.h4_ [ HH.text n , HH.text " ", HH.em_ [ HH.text $ showDisplayMode k ] ]
        , HH.p_ [ renderLabels lbls ]
        , case k of
            Samples -> renderChartTimeseries $ List.take 100 timeseries
            DiffSamples -> renderChartDiffTimeseries $ List.take 100 timeseries
            Smooth -> renderChartSmoothTimeseries $ timeseries
        , renderUnZoomButton idx
        , renderCycleChartSpec idx
        ]

    renderChart mts@(MultiTimeSeries idx _) =
      let 
          timeseries key = map (Map.lookup key <<< _.metrics) history
      in
      HH.div_
        [ HH.h4_ [ HH.em_ [ HH.text "combined plot" ] ]
        , renderUnZoomButton idx
        , renderMultiChartTimeseries
          $ map (List.take 100)
          $ map timeseries
          $ specKeys
          $ mts
        ]

    renderPromLine key =
      let n    = Tuple.fst key
          lbls = Tuple.snd key
          timeseries = map (Map.lookup key <<< _.metrics) history
      in
      HH.tr_ [ HH.td_ [ renderZoomButton n lbls
                      , renderMergeButon n lbls
                      ]
             , HH.td_ [ HH.text n ]
             , HH.td_ [ renderLabels lbls ]
             , HH.td_ [ renderSparkline timeseries ]
             , HH.td_ [ renderValues $ List.take 1 timeseries ]
             ]

    renderZoomButton n lbls =
      HH.button
        [ HP.type_ HP.ButtonSubmit
        , HE.onClick \_ -> Just (ZoomMetric n lbls)
        ]
        [ HH.text "+" ]

    renderUnZoomButton idx =
      HH.button
        [ HP.type_ HP.ButtonSubmit
        , HE.onClick \_ -> Just (UnZoomMetric idx)
        ]
        [ HH.text "-" ]

    renderMergeButon n lbls =
      HH.button
        [ HP.type_ HP.ButtonSubmit
        , HE.onClick \_ -> Just (MergeMetric n lbls)
        ]
        [ HH.text "$" ]

    renderCycleChartSpec idx =
      HH.button
        [ HP.type_ HP.ButtonSubmit
        , HE.onClick \_ -> Just (CycleChartSpec idx)
        ]
        [ HH.text "~" ]

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
    case st.polling of
      Nothing -> unpause
      Just _ -> pause
  where
    pause = HH.button
      [ HP.type_ HP.ButtonSubmit
      , HE.onClick \_ -> Just StopPolling
      ]
      [ HH.text "pause" ]

    unpause = HH.button
      [ HP.type_ HP.ButtonSubmit
      , HE.onClick \_ -> Just StartPolling
      ]
      [ HH.text "unpause" ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartPolling -> do
    sid <- H.subscribe timer
    H.modify_ _ { polling = Just sid }

  StopPolling -> do
    state <- H.get
    maybe (pure unit) (H.unsubscribe) state.polling
    H.modify_ _ { polling = Nothing }

  MakeStatusRequest event -> do
    response <- H.liftAff $ AX.get AXRF.string ("/status")
    H.modify_ _ { statusResult = map _.body (hush response) }

  MakeMetricsRequest -> do
    let url = "/metrics"
    response <- H.liftAff $ AX.get AXRF.string url
    let prom = hush response >>= parseBody
    let promlist = maybe Nil (\dat -> List.singleton $ fromPromDoc dat) prom
    H.modify_ \state -> state { metricsResult = map _.body (hush response)
                              , metricsHistory = List.take (state.nsamples + state.nextrasamples)
                                  $ promlist <> state.metricsHistory
                              }

  MergeMetric n lbls -> do
    H.modify_ \state -> 
      let idx = fromMaybe 0 (maximum (map specIndex state.displayedCharts))
          found = state.displayedCharts
                  # List.find (\cs -> specIndex cs == idx)
          cs2 = SingleTimeSeries (idx + 1) Samples n lbls

          merged = case found of
            Nothing -> cs2
            Just cs1@(SingleTimeSeries _ _ _ _) -> MultiTimeSeries (idx + 2) (List.fromFoldable [cs2,cs1])
            Just (MultiTimeSeries _ xs) -> MultiTimeSeries (idx + 2) (List.snoc xs cs2)
      in state { displayedCharts = state.displayedCharts
                     # removeChartSpec idx
                     # addChartSpec merged
               }

  ZoomMetric n lbls -> do
    H.modify_ \state -> 
      let idx = 1 + fromMaybe 0 (maximum (map specIndex state.displayedCharts))
          cs = SingleTimeSeries idx Samples n lbls
      in state { displayedCharts = addChartSpec cs state.displayedCharts }

  UnZoomMetric idx -> do
    H.modify_ \state -> state { displayedCharts = removeChartSpec idx state.displayedCharts }

  CycleChartSpec idx -> do
    H.modify_ \state -> 
      state { displayedCharts = cycleChartSpec idx state.displayedCharts }

removeChartSpec :: Int -> List ChartSpec -> List ChartSpec
removeChartSpec idx1 = List.filter different
  where
    different cs2 = idx1 /= specIndex cs2

addChartSpec :: ChartSpec -> List ChartSpec -> List ChartSpec
addChartSpec = flip List.snoc

cycleChartSpec :: Int -> List ChartSpec -> List ChartSpec
cycleChartSpec idx1 xs = map cycleOne xs
  where
    cycleOne spec@(MultiTimeSeries _ _) = spec
    cycleOne spec@(SingleTimeSeries idx2 k n lbls) =
      if (idx1 == idx2)
      then SingleTimeSeries idx1 (nextKind k) n lbls
      else spec

    nextKind = cycleDisplayMode

timer :: forall m. MonadAff m => EventSource m Action
timer = EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 500.0
    EventSource.emit emitter MakeMetricsRequest
  pure $ EventSource.Finalizer do
    Aff.killFiber (error "Event source finalized") fiber

parseBody :: forall t. { body :: String | t } -> Maybe PromDoc
parseBody = hush <<< (flip runParser) promDoc <<< _.body

