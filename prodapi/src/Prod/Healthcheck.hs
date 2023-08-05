module Prod.Healthcheck where

import Control.Monad (void, (>=>))
import Control.Concurrent (threadDelay)
import qualified Data.Either as Either
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Proxy (Proxy(..))

import qualified Prometheus as Prometheus
import Prod.Tracer (Tracer, contramap)
import Prod.Health (Readiness(..), GetReadinessApi)
import Prod.Background (BackgroundVal)
import qualified Prod.Background as Background
import qualified Prod.Discovery as Discovery

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
  , recentChecks :: [Either Error Check]
  }
  deriving Show

emptyCheckSummary :: CheckSummary
emptyCheckSummary = CheckSummary Nothing []

updateSummary :: Either Error Check -> CheckSummary -> CheckSummary
updateSummary v@(Right c) s
  | isSuccess c = CheckSummary (Just c) (v:(take 2 (recentChecks s)))
  | otherwise   = CheckSummary (lastReady s) (v:(take 2 (recentChecks s)))
updateSummary v@(Left _) s
                = CheckSummary (lastReady s) (v:(take 2 (recentChecks s)))

type CheckMap = Map (Host,Port) (BackgroundVal CheckSummary)

emptyCheckMap :: CheckMap
emptyCheckMap = Map.empty

initBackgroundCheck
  :: Counters
  -> Manager
  -> Tracer IO (Background.Track CheckSummary)
  -> (Host, Port)
  -> IO (BackgroundVal CheckSummary)
initBackgroundCheck cntrs manager tracer (h,p) =
    Background.background tracer emptyCheckSummary emptyCheckSummary step
  where
    step :: CheckSummary -> IO (CheckSummary, CheckSummary)
    step st0 = do
      Prometheus.incCounter (healthcheck_count cntrs)
      res <- check manager h p
      threadDelay 5000000
      let st1 = updateSummary res st0
      pure (st1, st1)

terminateBackgroundCheck :: BackgroundVal CheckSummary -> IO ()
terminateBackgroundCheck = Background.kill

data Runtime
  = Runtime
  { counters :: Counters
  , httpManager :: Manager
  , backgroundChecks :: IORef CheckMap
  , requestCheck :: (Host, Port) -> IO (BackgroundVal CheckSummary)
  , cancelCheck :: (Host, Port) -> IO ()
  }

data Counters
  = Counters
    { healthcheck_added :: !Prometheus.Counter
    , healthcheck_removed :: !Prometheus.Counter
    , healthcheck_count :: !Prometheus.Counter
    }

newCounters :: IO Counters
newCounters =
  Counters
    <$> counts "healthcheck_added"
    <*> counts "healthcheck_removed"
    <*> counts "healthchecks"
  where
    counts k =
      Prometheus.register $
        Prometheus.counter (Prometheus.Info k "")

initRuntime :: Tracer IO Track -> IO Runtime
initRuntime tracer = do
    r <- newIORef emptyCheckMap
    manager <- newManager defaultManagerSettings
    cntrs <- newCounters
    pure $ Runtime cntrs manager r (add cntrs r manager) (del cntrs r)
  where
    add :: Counters -> IORef CheckMap -> Manager -> (Host, Port) -> IO (BackgroundVal CheckSummary)
    add cntrs r manager = \hp -> do
      c <- Map.lookup hp <$> readIORef r
      case c of
        Nothing -> doadd cntrs r manager hp
        Just v -> pure v
    
    doadd :: Counters -> IORef CheckMap -> Manager -> (Host, Port) -> IO (BackgroundVal CheckSummary)
    doadd cntrs r manager = \hp@(h,p) -> do
      Prometheus.incCounter (healthcheck_added cntrs)
      c <- initBackgroundCheck cntrs manager (contramap (BackgroundTrack h p) tracer) hp
      concurrentlyAdded <- atomicModifyIORef' r (\st0 -> (Map.insertWith (\_ old -> old) hp c st0, Map.lookup hp st0))
      case concurrentlyAdded of
        Nothing -> pure c
        Just leader -> terminateBackgroundCheck c >> pure leader

    del :: Counters -> IORef CheckMap -> (Host, Port) -> IO ()
    del cntrs r = \hp -> do
      print ("removing", hp)
      Prometheus.incCounter (healthcheck_removed cntrs)
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

cancelDeadChecks :: Runtime -> IO ()
cancelDeadChecks rt = do
  summary <- readBackgroundChecks rt
  traverse_ (cancelCheck rt) (dead summary)

-- | Helper to build a Tracer to update hosts to check based on DNS-discovered answers.
-- Note that the DNSTrack only gives Host, so you need to fmap the port.
setChecksFromDNSDiscovery :: Runtime -> Discovery.DNSTrack [(Host,Port)] -> IO ()
setChecksFromDNSDiscovery rt (Discovery.DNSTrack _ _ (Discovery.BackgroundTrack (Background.RunDone _ newDNSResult))) =
  case Discovery.toMaybe newDNSResult of
    Just xs -> traverse_ (requestCheck rt) xs
    Nothing -> pure ()
setChecksFromDNSDiscovery hcrt _ = pure ()

-- | Same as 'setChecksFromDNSDiscovery' but only adding new checks.
-- You should clear checks of permanently invalid backends.
addChecksFromDNSDiscovery :: Runtime -> Discovery.DNSTrack [(Host,Port)] -> IO ()
addChecksFromDNSDiscovery rt (Discovery.DNSTrack _ _ (Discovery.BackgroundTrack (Background.RunDone _ newDNSResult))) =
  case Discovery.toMaybe newDNSResult of
    Just xs -> setChecks rt xs
    Nothing -> pure ()
addChecksFromDNSDiscovery hcrt _ = pure ()

type SummaryMap = Map (Host,Port) (CheckSummary)

readCheckMap :: CheckMap -> IO SummaryMap
readCheckMap = traverse Background.readBackgroundVal

readBackgroundChecks :: Runtime -> IO SummaryMap
readBackgroundChecks = readIORef . backgroundChecks >=> readCheckMap

-- | Returns the set of (Host,Port) that are healthy in a given SummaryMap.
--
-- Healthiness consists in having the latest healthcheck as healthy.
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
      $ Either.rights
      $ recentChecks c

dead :: SummaryMap -> [(Host, Port)]
dead m =
  fmap fst
  $ filter (\x -> neverHealthy x && healthChecked x)
  $ Map.toList m
  where
    healthChecked :: ((Host,Port), CheckSummary) -> Bool
    healthChecked (_, c) =
      length (recentChecks c) >= 3
    neverHealthy :: ((Host,Port), CheckSummary) -> Bool
    neverHealthy (_, c) =
      not
      $ any isSuccess
      $ Either.rights
      $ recentChecks c

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing
