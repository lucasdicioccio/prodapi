
module Prod.Watchdog where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Prod.Background (MicroSeconds, backgroundLoop, BackgroundVal)
import qualified Prod.Background
import Prod.Tracer (Tracer(..), contramap)
import Prometheus (Vector, Label1, Label2, Counter)
import qualified Prometheus as Prometheus
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Exception.Base (IOException, catch)
import System.Directory (setModificationTime, doesFileExist)

data Track = BackgroundTrack Prod.Background.Track
  deriving (Show)

data WatchdogResult a
  = Skipped
  | Success a
  | Failed
  deriving (Show, Ord, Eq)

data Watchdog a = Watchdog {
    backgroundVal :: BackgroundVal (WatchdogResult a)
  , tracer :: Tracer IO Track
  }

watchdog
  :: (Prometheus.Label label)
  => Vector label Counter
  -> Tracer IO Track
  -> (WatchdogResult a -> label)
  -> MicroSeconds Int
  -> IO (WatchdogResult a)
  -> IO (Watchdog a)
watchdog counters tracer mkLabel delay action =
    Watchdog <$> backgroundLoop (contramap BackgroundTrack tracer) Skipped go delay <*> pure tracer
  where
    go = do
      res <- action
      Prometheus.withLabel counters (mkLabel res) Prometheus.incCounter
      pure res

-- | Basic watchdog with a vector metric.
-- The input vector label is set with success|failed|skipped depending on the WatchdogResult.
basicWatchdog
  :: Vector Label1 Counter
  -> Tracer IO Track
  -> MicroSeconds Int
  -> IO (WatchdogResult a)
  -> IO (Watchdog a)
basicWatchdog counters tracer delay action =
    watchdog counters tracer basicLabel delay action

basicLabel :: WatchdogResult a -> Label1
basicLabel res = case res of
  Success _ -> "success"
  Failed    -> "failed"
  Skipped   -> "skipped"

data FileTouchTrack = FileTouchTrack FilePath Track
  deriving (Show)

-- | Touches a file periodically, using setModificationTime.
-- If the file does not exists when the watchdog is initialized, then it is
-- created empty.
fileTouchWatchdog
  :: FilePath
  -> Tracer IO FileTouchTrack
  -> MicroSeconds Int
  -> IO (Watchdog UTCTime)
fileTouchWatchdog path tracer delay = do
    let mkLabel res = (basicLabel res, Text.pack path)
    shouldCreate <- not <$> doesFileExist path
    when shouldCreate $ writeFile path ""
    watchdog fileTouchWatchdogCounter (contramap (FileTouchTrack path) tracer) mkLabel delay io
  where
    handleIOException :: IOException -> IO (WatchdogResult UTCTime)
    handleIOException _ = pure $ Failed
    io = do
      now <- getCurrentTime
      let touchFile = setModificationTime path now *> pure (Success now)
      touchFile `catch` handleIOException

{-# NOINLINE fileTouchWatchdogCounter #-}
fileTouchWatchdogCounter :: Vector Label2 Counter
fileTouchWatchdogCounter =
  Prometheus.unsafeRegister
    $ Prometheus.vector ("status", "path")
    $ Prometheus.counter (Prometheus.Info "prodapi_watchdog_filetouch" "")
