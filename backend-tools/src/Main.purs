module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Control.Monad.Rec.Class (forever)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), mapWithIndex)
import Data.List as List
import Data.Int (toNumber)
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
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.VDom.Driver (runUI)
import Web.UIEvent.MouseEvent (MouseEvent)
import Text.Parsing.Parser (runParser)

import Data.Foldable (fold, minimum, maximum, sum)
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

data ChartKind
  = Samples
  | DiffSamples
  | Smooth

data ChartSpec
  = SingleTimeSeries ChartKind MetricName Labels

type State =
  { statusResult :: Maybe String
  , metricsResult :: Maybe String
  , nsamples :: Int
  , metricsHistory :: List PromData
  , displayedCharts :: List ChartSpec
  }

data Action
  = MakeStatusRequest MouseEvent
  | MakeMetricsRequest
  | ZoomMetric MetricName Labels
  | UnZoomMetric MetricName Labels
  | CycleChartSpec MetricName Labels
  | Initialize

component
  :: forall query input output m. MonadAff m
  => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: forall i. i -> State
initialState _ =
  { statusResult: Nothing
  , metricsResult: Nothing
  , nsamples: 100
  , metricsHistory: Nil
  , displayedCharts: Nil
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

    renderChart (SingleTimeSeries k n lbls) =
      let key  = Tuple n lbls
          timeseries = map (Map.lookup key <<< _.metrics) history
      in
      HH.div_
        [ HH.h4_ [ HH.text n ]
        , HH.p_ [ renderLabels lbls ]
        , case k of
            Samples -> renderChartTimeseries timeseries
            DiffSamples -> renderChartDiffTimeseries timeseries
            Smooth -> renderChartSmoothTimeseries timeseries
        , renderUnZoomButton n lbls
        , renderCycleChartSpec n lbls
        ]

    renderPromLine key =
      let n    = Tuple.fst key
          lbls = Tuple.snd key
          timeseries = map (Map.lookup key <<< _.metrics) history
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

    renderUnZoomButton n lbls =
      HH.button
        [ HP.type_ HP.ButtonSubmit
        , HE.onClick \_ -> Just (UnZoomMetric n lbls)
        ]
        [ HH.text "-" ]

    renderCycleChartSpec n lbls =
      HH.button
        [ HP.type_ HP.ButtonSubmit
        , HE.onClick \_ -> Just (CycleChartSpec n lbls)
        ]
        [ HH.text "~" ]

    renderValues xs =
      HH.div_
        $ List.toUnfoldable
        $ map (\v -> HH.span_ [ HH.text $ v <> " "])
        $ map (maybe "NA" show) xs

renderSparkline xs =
 let reals = List.catMaybes xs
     vmin = minimum reals
     vmax = maximum reals
     normalize v = case (Tuple vmin vmax) of
        Tuple (Just v0) (Just v1) ->
          if v1 == v0
          then 5.0
          else (v - v0) / (v1 - v0)
        _ -> 5.0
     positionX idx = toNumber $ 305 - 3*idx
     positionY y = 20.0 * (1.0 - y)
     radius 0 = 3.0
     radius 1 = 2.0
     radius 2 = 1.5
     radius _ = 1.2
 in
 SE.svg [ SA.width 310.0
        , SA.height 20.0
        , SA.viewBox 0.0 0.0 310.0 20.0
        ]
    $ List.toUnfoldable
    $ mapWithIndex (\idx v ->
        SE.circle [ SA.cx $ positionX idx
                  , SA.cy $ positionY $ normalize v
                  , SA.r $ radius idx
                  , SA.fill (Just $ if idx == 0 then SA.RGB 200 0 0 else SA.RGB 100 100 100)
                  ])
    $ reals

renderChartTimeseries xs =
 let reals = List.catMaybes xs
     vmin = minimum reals
     vmax = maximum reals
     normalize v = case (Tuple vmin vmax) of
        Tuple (Just v0) (Just v1) ->
          if v1 == v0
          then 5.0
          else (v - v0) / (v1 - v0)
        _ -> 5.0
     positionX idx = toNumber $ 610 - 6*idx
     positionY y = 250.0 * (1.0 - y)
     radius 0 = 6.0
     radius 1 = 4.0
     radius 2 = 3.0
     radius _ = 2.4
     segments =
        List.toUnfoldable
        $ mapWithIndex (\idx (Tuple v1 v2) ->
            SE.g
              []
              [ SE.line
                  [ SA.x1 $ positionX idx
                  , SA.x2 $ positionX $ idx + 1
                  , SA.y1 $ positionY $ normalize v1
                  , SA.y2 $ positionY $ normalize v2
                  , SA.stroke (Just $ SA.RGBA 20 20 20 0.3)
                  , SA.strokeWidth 1.0
                  ]
               ])
        $ List.zip reals (List.drop 1 reals)
     points =
        List.toUnfoldable
        $ mapWithIndex (\idx v ->
            SE.g
              []
              [ SE.circle
                  [ SA.cx $ positionX idx
                  , SA.cy $ positionY $ normalize v
                  , SA.r $ radius idx
                  , SA.fill (Just $ if idx == 0 then SA.RGB 200 0 0 else SA.RGB 100 100 100)
                  ]
               ])
        $ reals
 in
 SE.svg [ SA.width 620.0
        , SA.height 250.0
        , SA.viewBox 0.0 0.0 620.0 250.0
        ]
        (segments <> points)

 
renderChartDiffTimeseries xs =
 let samples = List.catMaybes xs
     reals = List.zipWith (\s1 s0 -> s1 - s0) (List.drop 1 samples) samples

     vmin = minimum reals
     vmax = maximum reals
     normalize v = case (Tuple vmin vmax) of
        Tuple (Just v0) (Just v1) ->
          if v1 == v0
          then 5.0
          else (v - v0) / (v1 - v0)
        _ -> 5.0
     positionX idx = toNumber $ 610 - 6*idx
     positionY y = 250.0 * (1.0 - y)
     radius 0 = 6.0
     radius 1 = 4.0
     radius 2 = 3.0
     radius _ = 2.4

     segments =
        List.toUnfoldable
        $ mapWithIndex (\idx (Tuple v1 v2) ->
            SE.g
              []
              [ SE.line
                  [ SA.x1 $ positionX idx
                  , SA.x2 $ positionX $ idx + 1
                  , SA.y1 $ positionY $ normalize v1
                  , SA.y2 $ positionY $ normalize v2
                  , SA.stroke (Just $ SA.RGBA 20 20 20 0.3)
                  , SA.strokeWidth 1.0
                  ]
               ])
        $ List.zip reals (List.drop 1 reals)
     points =
        List.toUnfoldable
        $ mapWithIndex (\idx v ->
            SE.g
              []
              [ SE.circle
                  [ SA.cx $ positionX idx
                  , SA.cy $ positionY $ normalize v
                  , SA.r $ radius idx
                  , SA.fill (Just $ if idx == 0 then SA.RGB 200 0 0 else SA.RGB 100 100 100)
                  ]
               ])
        $ reals
 in
 SE.svg [ SA.width 620.0
        , SA.height 250.0
        , SA.viewBox 0.0 0.0 620.0 250.0
        ]
    $ segments <> points

renderChartSmoothTimeseries xs =
 let samples = List.catMaybes xs
     average zs = (sum zs) / (toNumber $ List.length zs)
     reals = 
       map average
       $ mapWithIndex (\idx _ -> List.take 30 $ List.drop idx $ samples) samples

     vmin = minimum reals
     vmax = maximum reals
     normalize v = case (Tuple vmin vmax) of
        Tuple (Just v0) (Just v1) ->
          if v1 == v0
          then 5.0
          else (v - v0) / (v1 - v0)
        _ -> 5.0
     positionX idx = toNumber $ 610 - 6*idx
     positionY y = 250.0 * (1.0 - y)
     radius 0 = 6.0
     radius 1 = 4.0
     radius 2 = 3.0
     radius _ = 2.4
     segments =
        List.toUnfoldable
        $ mapWithIndex (\idx (Tuple v1 v2) ->
            SE.g
              []
              [ SE.line
                  [ SA.x1 $ positionX idx
                  , SA.x2 $ positionX $ idx + 1
                  , SA.y1 $ positionY $ normalize v1
                  , SA.y2 $ positionY $ normalize v2
                  , SA.stroke (Just $ SA.RGBA 20 20 20 0.3)
                  , SA.strokeWidth 1.0
                  ]
               ])
        $ List.zip reals (List.drop 1 reals)
     points =
        List.toUnfoldable
        $ mapWithIndex (\idx v ->
            SE.g
              []
              [ SE.circle
                  [ SA.cx $ positionX idx
                  , SA.cy $ positionY $ normalize v
                  , SA.r $ radius idx
                  , SA.fill (Just $ if idx == 0 then SA.RGB 200 0 0 else SA.RGB 100 100 100)
                  ]
               ])
        $ reals
 in
 SE.svg [ SA.width 620.0
        , SA.height 250.0
        , SA.viewBox 0.0 0.0 620.0 250.0
        ]
    $ segments <> points



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
    ]
    [ HH.text "Metrics" ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.subscribe timer
    pure unit

  MakeStatusRequest event -> do
    response <- H.liftAff $ AX.get AXRF.string ("/status")
    H.modify_ _ { statusResult = map _.body (hush response) }

  MakeMetricsRequest -> do
    response <- H.liftAff $ AX.get AXRF.string ("/metrics")
    let prom = hush response >>= parseBody
    let promlist = maybe Nil (\dat -> List.singleton $ fromPromDoc dat) prom
    H.modify_ \state -> state { metricsResult = map _.body (hush response)
                              , metricsHistory = List.take state.nsamples
                                  $ promlist <> state.metricsHistory
                              }

  UnZoomMetric n lbls -> do
    H.modify_ \state -> state { displayedCharts = removeChartSpec n lbls state.displayedCharts }

  ZoomMetric n lbls -> do
    let cs = SingleTimeSeries Samples n lbls
    H.modify_ \state -> state { displayedCharts = addChartSpec cs state.displayedCharts }

  CycleChartSpec n lbls -> do
    H.modify_ \state -> state { displayedCharts = cycleChartSpec n lbls state.displayedCharts }

removeChartSpec :: MetricName -> Labels -> List ChartSpec -> List ChartSpec
removeChartSpec n1 lbls1 = List.filter different
  where
    different (SingleTimeSeries _ n2 lbls2) = not $ n2 == n1 && lbls1 == lbls2

addChartSpec :: ChartSpec -> List ChartSpec -> List ChartSpec
addChartSpec = flip List.snoc

cycleChartSpec :: MetricName -> Labels -> List ChartSpec -> List ChartSpec
cycleChartSpec n1 lbls1 xs = map cycleOne xs
  where
    cycleOne spec@(SingleTimeSeries k n2 lbls2) =
      if (n2 == n1 && lbls1 == lbls2)
      then SingleTimeSeries (nextKind k) n1 lbls1
      else spec

    nextKind Samples = DiffSamples
    nextKind DiffSamples = Smooth
    nextKind Smooth = Samples


timer :: forall m. MonadAff m => EventSource m Action
timer = EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 500.0
    EventSource.emit emitter MakeMetricsRequest
  pure $ EventSource.Finalizer do
    Aff.killFiber (error "Event source finalized") fiber

parseBody :: forall t. { body :: String | t } -> Maybe PromDoc
parseBody = hush <<< (flip runParser) promDoc <<< _.body

