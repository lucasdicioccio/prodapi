
module Parsing.Prometheus
 ( promDoc
 , promLine
 , fromArray
 , PromDoc
 , Line(..)
 , MetricName
 , Labels
 , LabelPair
 , pairName
 , pairValue
 , LabelName
 , LabelValue
 , TypeName
 , MetricValue
 , MetricTimestamp
 ) where

import Prelude (class Show, Unit, bind, mempty, negate, pure, ($), (*>), (<$>), (<*), (<>))

import Global (nan, infinity)
import Control.Alt ((<|>))
import Data.Float.Parse (parseFloat)
import Data.Array (many)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (option, optionMaybe, sepBy, sepEndBy, skipMany1, try)
import Text.Parsing.Parser.String

type MetricName = String
type LabelName = String
type TypeName = String
type LabelValue = String
type MetricValue = Number
type MetricTimestamp = String

type LabelPair = Tuple LabelName LabelValue
type Labels = List LabelPair

pairName :: LabelPair -> LabelName
pairName = fst

pairValue :: LabelPair -> LabelValue
pairValue = snd

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

fromArray :: Array Line -> PromDoc
fromArray = List.fromFoldable

promDoc :: Parser String PromDoc
promDoc = promLine `sepBy` char '\n'

promLine :: Parser String Line
promLine =
      metric
  <|> try helpLine
  <|> try typeLine
  <|> try comment
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
  char '{' *> labelsList <* char '}'

labelsList :: Parser String (List LabelPair)
labelsList = labelPair `sepEndBy` string ","

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
      boundedFloat
  <|> (string "NaN" *> pure nan)
  <|> (string "+Inf" *> pure infinity)
  <|> (string "-Inf" *> pure (-infinity))


allowedFloatChars :: Array Char
allowedFloatChars = toCharArray "01234567890-e."

boundedFloat :: Parser String Number
boundedFloat = do
  val <- fromCharArray <$> many (oneOf allowedFloatChars)
  case parseFloat val of
    Just num -> pure num
    _        -> fail $ "could not parse float from val: " <> val

allowedTimestampChars :: Array Char
allowedTimestampChars = toCharArray "1234567890-"

metricTimestamp :: Parser String MetricTimestamp
metricTimestamp = fromCharArray <$> many (oneOf allowedTimestampChars)

allowedNameChars :: Array Char
allowedNameChars = toCharArray "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm_1234567890"

promBasicName :: Parser String (Array Char)
promBasicName =
  many (oneOf allowedNameChars)

promQuotedString :: Parser String (Array Char)
promQuotedString =
  many (escapedChar <|> otherChar)

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

