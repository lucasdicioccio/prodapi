module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Float.Parse (parseFloat)
import Data.Either (hush, Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), filter, toUnfoldable)
import Data.Tuple (Tuple(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String
import Control.Alt ((<|>))
import Data.Array (many)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Global (nan, infinity)

type MetricName = String
type LabelName = String
type TypeName = String
type LabelValue = String
type MetricValue = Number
type MetricTimestamp = String

type LabelPair = Tuple LabelName LabelValue
type Labels = List LabelPair

data Line
 = MetricLine MetricName Labels MetricValue (Maybe MetricTimestamp)
 | TypeLine MetricName TypeName
 | HelpLine MetricName String
 | CommentLine String
 | OtherLine String

derive instance genericLine :: Generic Line _

instance showLine :: Show Line where
  show = genericShow

type PromDoc = List Line

promDoc :: Parser String PromDoc
promDoc = promLine `sepBy` char '\n'

promLine =
  try helpLine
  <|> try typeLine
  <|> try comment
  <|> try metric
  <|> (OtherLine <$> restOfLine)

metric :: Parser String Line
metric = do
  n <- metricName
  pairs <- option mempty labels
  _ <- spacing
  val <- metricValue
  tst <- optionMaybe (spacing *> metricTimestamp)
  _ <- restOfLine
  pure $ MetricLine n pairs val tst

spacing :: Parser String Unit
spacing = skipMany1 (char ' ')

labels :: Parser String (List LabelPair)
labels = do
  char '{' *> lbls <* char '}'
  where
    lbls = labelPair `sepEndBy` string ","

labelPair :: Parser String LabelPair
labelPair = do
  n <- labelName
  _ <- char '='
  _ <- char '"'
  v <- labelValue
  _ <- char '"'
  pure $ Tuple n v

restOfLine :: Parser String String
restOfLine = fromCharArray <$> many (noneOf ['\n'])

metricName :: Parser String MetricName
metricName = fromCharArray <$> promBasicName

labelName :: Parser String LabelName
labelName = fromCharArray <$> promBasicName

labelValue :: Parser String LabelValue
labelValue = fromCharArray <$> promQuotedString

metricValue :: Parser String MetricValue
metricValue = 
  try (string "+Inf" *> pure infinity)
  <|> try (string "-Inf" *> pure (-infinity))
  <|> try (string "NaN" *> pure nan)
  <|> boundedFloat

boundedFloat :: Parser String Number
boundedFloat = do
  val <- fromCharArray <$> many (oneOf $ toCharArray "01234567890-e.")
  case parseFloat val of
    Just num -> pure num
    _        -> fail $ "could not parse float from val: " <> val

metricTimestamp :: Parser String MetricTimestamp
metricTimestamp = fromCharArray <$> many (oneOf $ toCharArray "1234567890-")

promBasicName :: Parser String (Array Char)
promBasicName =
  many (oneOf $ toCharArray "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm_1234567890")

promQuotedString :: Parser String (Array Char)
promQuotedString =
  many (try escapedChar <|> otherChar)
  where
    escapedChar :: Parser String Char
    escapedChar = char '\\' *> oneOf (toCharArray "\\\"\n")

    otherChar :: Parser String Char
    otherChar = noneOf $ toCharArray "\""


helpLine :: Parser String Line
helpLine = do
  _ <- string "# HELP "
  n <- metricName
  _ <- char ' '
  c <- restOfLine
  pure $ HelpLine n c

typeLine :: Parser String Line
typeLine = do
  _ <- string "# TYPE "
  n <- metricName
  _ <- char ' '
  c <- restOfLine
  pure $ TypeLine n c

comment :: Parser String Line
comment = CommentLine <$> (string "# " *> restOfLine)


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

