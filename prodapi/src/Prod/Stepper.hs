{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | A simplistic stepper for delayable singly-threaded state machines.
module Prod.Stepper where

import qualified Control.Concurrent.Thread.Delay as ThreadDelay
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.UnixTime
import Foreign.C.Types (CTime (..))
import GHC.Generics (Generic)

import Prod.Tracer (Tracer (..), runTracer)

-------------------------------------------------------------------------------
data DelaySpec
    = DelaySafetyAmount
    | DelayUntil UTCTime
    | DelayUntilEpochInteger Int64
    deriving (Show, Eq, Ord, Generic)

instance ToJSON DelaySpec
instance FromJSON DelaySpec

type SafetySeconds = Int64

waitUntil :: SafetySeconds -> DelaySpec -> IO ()
waitUntil _ (DelayUntilEpochInteger time) = do
    now <- getUnixTime
    let diff = UnixTime (coerce time) 0 `diffUnixTime` now
    let delayV = udtSeconds diff + 10
    when (delayV > 0) $
        ThreadDelay.delay (toInteger (coerce $ delayV * 1000000 :: Int64))
waitUntil _ (DelayUntil time) = do
    now <- getCurrentTime
    let diff = time `diffUTCTime` now
    let delayV = round (nominalDiffTimeToSeconds diff + 10) :: Int64
    when (delayV > 0) $
        ThreadDelay.delay (toInteger (coerce $ delayV * 1000000 :: Int64))
waitUntil amount DelaySafetyAmount =
    when (amount > 0) $
        ThreadDelay.delay (toInteger (coerce $ amount * 1000000 :: Int64))

isExpired :: DelaySpec -> IO Bool
isExpired (DelayUntilEpochInteger time) = do
    now <- getUnixTime
    let diff = UnixTime (coerce time) 0 `diffUnixTime` now
    let delayV = udtSeconds diff + 10
    pure (delayV <= 0)
isExpired (DelayUntil time) = do
    now <- getCurrentTime
    let diff = time `diffUTCTime` now
    let delayV = round (nominalDiffTimeToSeconds diff + 10) :: Int64
    pure (delayV <= 0)
isExpired DelaySafetyAmount =
    pure False

-------------------------------------------------------------------------------
data Trace input output
    = Start input
    | Suspend DelaySpec input
    | Finish input
    | Delaying DelaySpec input output
    | Inlining input output
    deriving (Show, Eq, Ord, Generic)

instance (ToJSON i, ToJSON o) => ToJSON (Trace i o)
instance (FromJSON i, FromJSON o) => FromJSON (Trace i o)

-------------------------------------------------------------------------------
type BaseStepIO a b = (b -> IO ()) -> a -> IO ()

mapInput :: (c -> a) -> BaseStepIO a b -> BaseStepIO c b
mapInput f g = \consumeA cVal -> g consumeA (f cVal)

mapOutput :: (b -> z) -> BaseStepIO a b -> BaseStepIO a z
mapOutput f g = \consumeZ bVal -> g (consumeZ . f) bVal

-------------------------------------------------------------------------------
data Delayable a = Inline a | Delay DelaySpec a
    deriving (Show, Functor)

-- | Turn a pair of next function into a handler of Delayable.
nextStep :: (DelaySpec -> t1 -> t2) -> (t1 -> t2) -> Delayable t1 -> t2
nextStep _ inlineF (Inline val) = inlineF val
nextStep delayF _ (Delay d val) = delayF d val

-------------------------------------------------------------------------------
type StepIO a b = BaseStepIO (Delayable a) (Delayable b)

-- special case for a StepIO that will start immediately (removing the need to handle the Delayable branch)
type StartStepIO a b = BaseStepIO a (Delayable b)

-------------------------------------------------------------------------------
data ExecFunctions arg = ExecFunctions
    { inline :: arg -> IO ()
    , delay :: DelaySpec -> arg -> IO ()
    }

defineExecution ::
    -- | tracer to save the state
    Tracer IO (Trace z1 z2) ->
    -- | function to turn an input into a state we hope to resume
    (a -> z1) ->
    (b -> z2) ->
    -- | handler-defining function with a bag of delay/inline functions and a start value
    (ExecFunctions b -> a -> IO ()) ->
    StepIO a b
defineExecution tracer f g runStep = \next input -> do
    -- callback given to downstream consumer: so that we can delay
    let delayF val delaySpec nextObj = do
            let mappedOutput = g nextObj
            runTracer tracer (Delaying delaySpec val mappedOutput)
            next (Delay delaySpec nextObj)

    -- callback given to downstream consumer: so that we can inline
    let inlineF val nextObj = do
            let mappedOutput = g nextObj
            runTracer tracer (Inlining val mappedOutput)
            next (Inline nextObj)

    -- hanling of input
    case input of
        Inline val -> do
            let mappedInput = f val
            runTracer tracer (Start mappedInput)
            runStep (ExecFunctions (inlineF mappedInput) (delayF mappedInput)) val
            runTracer tracer (Finish mappedInput)
        Delay delaySpec val -> do
            let mappedInput = f val
            runTracer tracer (Suspend delaySpec mappedInput)

onSuspend :: (DelaySpec -> a -> IO ()) -> Tracer IO (Trace a b)
onSuspend action =
    Tracer go
  where
    go (Suspend delaySpec obj) = do
        action delaySpec obj
    go _ = do
        pure ()
