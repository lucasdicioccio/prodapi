
module Monitor where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Either (Either, hush)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map
import Data.Map (Map)
import Data.List (List(..))
import Data.List as List
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as EventSource
import Text.Parsing.Parser (runParser)

import Data.Foldable (maximum)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Charting.Charts (ChartSpec(..), specIndex, ChartDisplayMode(..), cycleDisplayMode) 
import Charting.SparkLine (renderSparkline)
import Charting.TimeSeries (renderChartTimeseries, renderChartDiffTimeseries)

import Parsing.Prometheus (promDoc, PromDoc, Labels, LabelPair, pairName, pairValue, MetricName, MetricValue)
import History (History, emptyHistory, lookupHistory, hdToList, updateHistory, historyKeys)
import HistoryPacked as PH

showDisplayMode :: ChartDisplayMode -> String
showDisplayMode = case _ of
  Samples -> "raw"
  DiffSamples -> "delta"

type State =
  { metricsResult :: Maybe String
  , nsamples :: Int
  , nextrasamples :: Int
  , displayedCharts :: List ChartSpec
  , polling :: Maybe H.SubscriptionId
  , pollingPeriod :: Milliseconds
  , metricsRequest :: Effect (AX.Request String)
  , history :: History
  , packedHistory :: PH.History
  , historyData :: Map PH.HistoryKey PH.HistoryData
  }

data Action
  = MakeMetricsRequest
  | ZoomMetric MetricName Labels
  | UnZoomMetric Int
  | CycleChartSpec Int
  | StartPolling Milliseconds
  | StopPolling

component
  :: forall query input output m. MonadAff m
  => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ StartPolling $ Milliseconds 1000.0
        }
    }

initialState :: forall i. i -> State
initialState _ =
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
    defaultMakeRequest =
      pure AX.defaultRequest
              { url = "/metrics"
              , responseFormat = AXRF.string 
              , timeout = Just $ Milliseconds 250.0 
              }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div_
    [ renderButtons st
    , renderZoomedCharts st
    , renderSparkTable st
    ]

renderZoomedCharts :: forall m. State -> H.ComponentHTML Action () m
renderZoomedCharts st =
  HH.section_
    $ map (\spec -> renderChart spec)
    $ List.toUnfoldable
    $ st.displayedCharts
  where
    renderChart (TimeSeries idx k n lbls) =
      let key  = Tuple n lbls
          timeseries = Map.lookup key st.historyData
            # map (List.fromFoldable)
            # fromMaybe Nil
      in
      HH.div_
        [ HH.h4_ [ HH.text n , HH.text " ", HH.em_ [ HH.text $ showDisplayMode k ] ]
        , HH.p_ [ renderLabels lbls ]
        , case k of
            Samples -> renderChartTimeseries $ List.take 100 timeseries
            DiffSamples -> renderChartDiffTimeseries $ List.take 100 timeseries
        , renderUnZoomButton idx
        , renderCycleChartSpec idx
        ]
    renderUnZoomButton idx =
      HH.button
        [ HP.type_ HP.ButtonSubmit
        , HE.onClick \_ -> Just (UnZoomMetric idx)
        ]
        [ HH.text "-" ]

    renderCycleChartSpec idx =
      HH.button
        [ HP.type_ HP.ButtonSubmit
        , HE.onClick \_ -> Just (CycleChartSpec idx)
        ]
        [ HH.text "~" ]

renderSparkTable
  :: forall m
  .  State
  -> H.ComponentHTML Action () m
renderSparkTable st =
      HH.table_
        $ map (\key -> renderPromLine key)
        $ PH.historyKeys st.packedHistory
  where
    renderPromLine key =
      let n    = Tuple.fst key
          lbls = Tuple.snd key
          timeseries = Map.lookup key st.historyData
            # map (List.fromFoldable)
            # fromMaybe Nil
      in
      HH.tr_ [ HH.td_ [ renderZoomButton n lbls ]
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

renderButtons :: forall m. State -> H.ComponentHTML Action () m
renderButtons st =
    case st.polling of
      Nothing -> unpause
      Just _ -> HH.div_ [ pause , hasten , slowdown ]
  where
    pause = HH.button
      [ HP.type_ HP.ButtonSubmit
      , HE.onClick \_ -> Just StopPolling
      ]
      [ HH.text "pause" ]

    unpause = HH.button
      [ HP.type_ HP.ButtonSubmit
      , HE.onClick \_ -> Just $ StartPolling st.pollingPeriod
      ]
      [ HH.text "unpause" ]

    Milliseconds ms = st.pollingPeriod

    hasten = HH.button
      [ HP.type_ HP.ButtonSubmit
      , HE.onClick \_ -> Just $ StartPolling (Milliseconds $ ms / 2.0)
      ]
      [ HH.text "hasten" ]

    slowdown = HH.button
      [ HP.type_ HP.ButtonSubmit
      , HE.onClick \_ -> Just $ StartPolling (Milliseconds $ ms * 2.0)
      ]
      [ HH.text "tame" ]


getMetrics :: forall action input output m. MonadAff m
 => H.HalogenM State action input output m (Either AX.Error (AX.Response String))
getMetrics = do
    state <- H.get
    req <- liftEffect $ state.metricsRequest
    H.liftAff $ AX.request req


handleAction :: forall output m. MonadAff m
 => Action
 -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartPolling ms -> do
    state <- H.get
    maybe (pure unit) (H.unsubscribe) state.polling
    sid <- H.subscribe $ timer ms
    H.modify_ _ { polling = Just sid , pollingPeriod = ms }

  StopPolling -> do
    state <- H.get
    maybe (pure unit) (H.unsubscribe) state.polling
    H.modify_ _ { polling = Nothing }

  MakeMetricsRequest -> do
    response <- getMetrics
    let prom = hush response >>= parseBody
    state <- H.get
    ph <- liftEffect $ PH.updateHistory' (map PH.fromPromDoc prom) state.packedHistory
    am <- liftEffect $ PH.toArrayMap ph
    H.modify_ \st -> st { metricsResult = map _.body (hush response)
                        , packedHistory = ph
                        , historyData = am
                        }

  ZoomMetric n lbls -> do
    H.modify_ \state -> 
      let idx = 1 + fromMaybe 0 (maximum (map specIndex state.displayedCharts))
          cs = TimeSeries idx Samples n lbls
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
    cycleOne spec@(TimeSeries idx2 k n lbls) =
      if (idx1 == idx2)
      then TimeSeries idx1 (nextKind k) n lbls
      else spec

    nextKind = cycleDisplayMode

timer :: forall m. MonadAff m => Milliseconds -> EventSource m Action
timer ms = EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    EventSource.emit emitter MakeMetricsRequest
    Aff.delay ms
  pure $ EventSource.Finalizer do
    Aff.killFiber (error "Event source finalized") fiber

parseBody :: forall t. { body :: String | t } -> Maybe PromDoc
parseBody = hush <<< (flip runParser) promDoc <<< _.body

