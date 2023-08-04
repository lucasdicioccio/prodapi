module Prod.Healthcheck where

import Control.Monad (void, (>=>))
import Control.Concurrent (threadDelay)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Proxy (Proxy(..))

import Prod.Tracer (Tracer, contramap)
import Prod.Health (Readiness(..), GetReadinessApi)
import Prod.Background (BackgroundVal)
import qualified Prod.Background as Background

import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import qualified Servant.Client as ServantClient

type Host = Text
type Port = Int
type Error = Text

data Track
  = HealthCheckStarted Host Port
  | HealthCheckFinished Host Port Check
  | BackgroundTrack Host Port (Background.Track CheckSummary)
  deriving Show

data Check
  = Success UTCTime Readiness
  | Failed UTCTime Error
  deriving Show

resultTime :: Check -> UTCTime
resultTime (Success t _) = t
resultTime (Failed t _) = t

isSuccess :: Check -> Bool
isSuccess (Success _ Ready) = True
isSuccess _                 = False

getReadiness :: ServantClient.ClientM Readiness
getReadiness = ServantClient.client (Proxy @GetReadinessApi)

check :: Manager -> Host -> Port -> IO (Either Error Check)
check httpManager host port = do
  now <- getCurrentTime
  let env = ServantClient.mkClientEnv httpManager (ServantClient.BaseUrl ServantClient.Http (Text.unpack host) port "")
  r <- ServantClient.runClientM getReadiness env
  case r of
    Left err -> pure $ Left $ Text.pack $ show err
    Right v -> pure $ Right (Success now v)

data CheckSummary
  = CheckSummary
  { lastReady    :: Maybe Check
  , recentChecks :: [Check]
  }
  deriving Show

emptyCheckSummary :: CheckSummary
emptyCheckSummary = CheckSummary Nothing []

summaryTime :: CheckSummary -> Maybe UTCTime
summaryTime s = resultTime <$> safeHead (recentChecks s)
updateSummary :: Check -> CheckSummary -> CheckSummary
updateSummary c s
  | isSuccess c = CheckSummary (Just c) (c:(take 2 (recentChecks s)))
  | otherwise   = CheckSummary (lastReady s) (c:(take 2 (recentChecks s)))

type CheckMap = Map (Host,Port) (BackgroundVal CheckSummary)

emptyCheckMap :: CheckMap
emptyCheckMap = Map.empty

initBackgroundCheck
  :: Manager
  -> Tracer IO (Background.Track CheckSummary)
  -> (Host, Port)
  -> IO (BackgroundVal CheckSummary)
initBackgroundCheck manager tracer (h,p) =
    Background.background tracer emptyCheckSummary emptyCheckSummary step
  where
    step :: CheckSummary -> IO (CheckSummary, CheckSummary)
    step st0 = do
      res <- check manager h p
      threadDelay 5000000
      case res of
        Left err -> pure (st0, st0)
        Right c -> let st1 = updateSummary c st0 in pure (st1, st1)

terminateBackgroundCheck :: BackgroundVal CheckSummary -> IO ()
terminateBackgroundCheck = Background.kill

data Runtime
  = Runtime
  { httpManager :: Manager
  , backgroundChecks :: IORef CheckMap
  , requestCheck :: (Host, Port) -> IO (BackgroundVal CheckSummary)
  , cancelCheck :: (Host, Port) -> IO ()
  }

initRuntime :: Tracer IO Track -> IO Runtime
initRuntime tracer = do
    r <- newIORef emptyCheckMap
    manager <- newManager defaultManagerSettings
    pure $ Runtime manager r (add r manager) (del r)
  where
    add :: IORef CheckMap -> Manager -> (Host, Port) -> IO (BackgroundVal CheckSummary)
    add r manager = \hp@(h,p) -> do
      c <- initBackgroundCheck manager (contramap (BackgroundTrack h p) tracer) hp
      concurrentlyAdded <- atomicModifyIORef' r (\st0 -> (Map.insertWith (\_ old -> old) hp c st0, Map.lookup hp st0))
      case concurrentlyAdded of
        Nothing -> pure c
        Just leader -> terminateBackgroundCheck c >> pure leader

    del :: IORef CheckMap -> (Host, Port) -> IO ()
    del r = \hp -> do
      c <- atomicModifyIORef' r (\st0 -> (Map.delete hp st0, Map.lookup hp st0))
      case c of
        Nothing -> pure ()
        Just b -> terminateBackgroundCheck b

setChecks :: Runtime -> [(Host,Port)] -> IO ()
setChecks rt hps = do
  let wantedSet = Set.fromList hps
  currentSet <- Map.keysSet <$> readIORef (backgroundChecks rt)
  let spurious = currentSet `Set.difference` wantedSet
  let missing = wantedSet `Set.difference` currentSet
  traverse_ (cancelCheck rt) spurious
  traverse_ (requestCheck rt) missing

type SummaryMap = Map (Host,Port) (CheckSummary)

readCheckMap :: CheckMap -> IO SummaryMap
readCheckMap = traverse Background.readBackgroundVal

readBackgroundChecks :: Runtime -> IO SummaryMap
readBackgroundChecks = readIORef . backgroundChecks >=> readCheckMap

healthy :: SummaryMap -> [(Host, Port)]
healthy m =
  fmap fst
  $ filter recentlyHealthy
  $ Map.toList m
  where
    recentlyHealthy :: ((Host,Port), CheckSummary) -> Bool
    recentlyHealthy (_, c) =
      maybe False isSuccess
      $ safeHead
      $ recentChecks c

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

