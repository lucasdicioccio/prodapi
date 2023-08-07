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

-- | Predicate to tell if a Summary contains a long-enough check history to be considered.
healthChecked :: CheckSummary -> Bool
healthChecked c =
  length (recentChecks c) >= 3

-- | Predicate to tell if a Summary contains no recent successful healthcheck.
neverHealthy :: CheckSummary -> Bool
neverHealthy c =
  not
  $ any isSuccess
  $ Either.rights
  $ recentChecks c

-- | Predicate to tell if the most recent summary exists and is successful.
recentlyHealthy :: CheckSummary -> Bool
recentlyHealthy c =
  maybe False isSuccess
  $ safeHead
  $ Either.rights
  $ recentChecks c



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
  :: SpaceCounters
  -> Manager
  -> Tracer IO (Background.Track CheckSummary)
  -> (Host, Port)
  -> IO (BackgroundVal CheckSummary)
initBackgroundCheck cntrs manager tracer (h,p) =
    Background.background tracer emptyCheckSummary emptyCheckSummary step
  where
    step :: CheckSummary -> IO (CheckSummary, CheckSummary)
    step st0 = do
      ns_healthcheck_count cntrs $ Prometheus.incCounter 
      res <- check manager h p
      threadDelay 5000000
      let st1 = updateSummary res st0
      pure (st1, st1)

terminateBackgroundCheck :: BackgroundVal CheckSummary -> IO ()
terminateBackgroundCheck = Background.kill

data Space
  = Space
  { spacehttpManager :: Manager
  , backgroundChecks :: IORef CheckMap
  , requestCheck :: (Host, Port) -> IO (BackgroundVal CheckSummary)
  , cancelCheck :: (Host, Port) -> IO ()
  }

clearSpace :: Space -> IO ()
clearSpace sp = do
  v <- atomicModifyIORef' (backgroundChecks sp) (\old -> (Map.empty, old))
  traverse_ terminateBackgroundCheck v

data Counters
  = Counters
    { healthcheck_added :: !(Prometheus.Vector Text Prometheus.Counter)
    , healthcheck_removed :: !(Prometheus.Vector Text Prometheus.Counter)
    , healthcheck_count :: !(Prometheus.Vector Text Prometheus.Counter)
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
        Prometheus.vector "ns" $
          Prometheus.counter (Prometheus.Info k "")

type WithSpaceCounter = (Prometheus.Counter -> IO ()) -> IO ()

data SpaceCounters
  = SpaceCounters
    { ns_healthcheck_added :: WithSpaceCounter
    , ns_healthcheck_removed :: WithSpaceCounter
    , ns_healthcheck_count :: WithSpaceCounter
    }

namespaceCounters :: Namespace -> Counters -> SpaceCounters
namespaceCounters ns cntrs =
  SpaceCounters
    (withNamespace (healthcheck_added cntrs))
    (withNamespace (healthcheck_removed cntrs))
    (withNamespace (healthcheck_count cntrs))
  where
    withNamespace v f = Prometheus.withLabel v ns f


initSpace :: SpaceCounters -> Manager -> Tracer IO Track -> IO Space
initSpace cntrs manager tracer = do
    r <- newIORef emptyCheckMap
    pure $ Space manager r (add cntrs r manager) (del cntrs r)
  where
    add :: SpaceCounters -> IORef CheckMap -> Manager -> (Host, Port) -> IO (BackgroundVal CheckSummary)
    add cntrs r manager = \hp -> do
      c <- Map.lookup hp <$> readIORef r
      case c of
        Nothing -> doadd cntrs r manager hp
        Just v -> pure v
    
    doadd :: SpaceCounters -> IORef CheckMap -> Manager -> (Host, Port) -> IO (BackgroundVal CheckSummary)
    doadd cntrs r manager = \hp@(h,p) -> do
      ns_healthcheck_added cntrs $ Prometheus.incCounter 
      c <- initBackgroundCheck cntrs manager (contramap (BackgroundTrack h p) tracer) hp
      concurrentlyAdded <- atomicModifyIORef' r (\st0 -> (Map.insertWith (\_ old -> old) hp c st0, Map.lookup hp st0))
      case concurrentlyAdded of
        Nothing -> pure c
        Just leader -> terminateBackgroundCheck c >> pure leader

    del :: SpaceCounters -> IORef CheckMap -> (Host, Port) -> IO ()
    del cntrs r = \hp -> do
      print ("removing", hp)
      ns_healthcheck_removed cntrs $ Prometheus.incCounter 
      c <- atomicModifyIORef' r (\st0 -> (Map.delete hp st0, Map.lookup hp st0))
      case c of
        Nothing -> pure ()
        Just b -> terminateBackgroundCheck b

setChecks :: Space -> [(Host,Port)] -> IO ()
setChecks space hps = do
  let wantedSet = Set.fromList hps
  currentSet <- Map.keysSet <$> readIORef (backgroundChecks space)
  let spurious = currentSet `Set.difference` wantedSet
  let missing = wantedSet `Set.difference` currentSet
  traverse_ (cancelCheck space) spurious
  traverse_ (requestCheck space) missing

cancelDeadChecks :: Space -> IO ()
cancelDeadChecks space = do
  summary <- readBackgroundChecks space
  traverse_ (cancelCheck space) (deadKeys summary)

-- | Helper to build a Tracer to update hosts to check based on DNS-discovered answers.
-- Note that the DNSTrack only gives Host, so you need to fmap the port.
setChecksFromDNSDiscovery :: Space -> Discovery.DNSTrack [(Host,Port)] -> IO ()
setChecksFromDNSDiscovery space (Discovery.DNSTrack _ _ (Discovery.BackgroundTrack (Background.RunDone _ newDNSResult))) =
  case Discovery.toMaybe newDNSResult of
    Just xs -> traverse_ (requestCheck space) xs
    Nothing -> pure ()
setChecksFromDNSDiscovery hcrt _ = pure ()

-- | Same as 'setChecksFromDNSDiscovery' but only adding new checks.
-- You should clear checks of permanently invalid backends.
addChecksFromDNSDiscovery :: Space -> Discovery.DNSTrack [(Host,Port)] -> IO ()
addChecksFromDNSDiscovery space (Discovery.DNSTrack _ _ (Discovery.BackgroundTrack (Background.RunDone _ newDNSResult))) =
  case Discovery.toMaybe newDNSResult of
    Just xs -> setChecks space xs
    Nothing -> pure ()
addChecksFromDNSDiscovery hcrt _ = pure ()

type SummaryMap = Map (Host,Port) (CheckSummary)

readCheckMap :: CheckMap -> IO SummaryMap
readCheckMap = traverse Background.readBackgroundVal

readBackgroundChecks :: Space -> IO SummaryMap
readBackgroundChecks = readIORef . backgroundChecks >=> readCheckMap

-- | Returns the set of (Host,Port) that are healthy in a given SummaryMap.
--
-- Healthiness consists in having the latest healthcheck as healthy.
healthyKeys :: SummaryMap -> [(Host, Port)]
healthyKeys m =
  fmap fst
  $ filter (recentlyHealthy . snd)
  $ Map.toList m

-- | Returns the set of (Host,Port) that have no recent successful activity
-- provided there is enough health-checking history.
deadKeys :: SummaryMap -> [(Host, Port)]
deadKeys m =
  fmap fst
  $ filter (\(_,x) -> neverHealthy x && healthChecked x)
  $ Map.toList m

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

type Namespace = Text

type Namespaced a = (Namespace, a)

data Runtime
  = Runtime
  { counters :: Counters
  , httpManager :: Manager
  , tracer :: Tracer IO (Namespaced Track)
  -- todo: split globals env values and dynamic-space storage
  , spaces :: IORef (Map Namespace Space)
  }

initRuntime :: Tracer IO (Namespaced Track) -> IO Runtime
initRuntime tracer = do
  r <- newIORef Map.empty
  manager <- newManager defaultManagerSettings
  cntrs <- newCounters
  pure $ Runtime cntrs manager tracer r

registerSpace :: Runtime -> Namespace -> IO Space
registerSpace rt ns = withSpace rt ns pure

withSpace :: Runtime -> Namespace -> (Space -> IO a) -> IO a
withSpace rt ns run = do
  let r = spaces rt
  sp <- Map.lookup ns <$> readIORef r
  case sp of
    Just s -> run s
    Nothing -> do
      s <- initRuntimeSpace rt ns
      concurrentlyAdded <- atomicModifyIORef' r (\st0 -> (Map.insertWith (\_ old -> old) ns s st0, Map.lookup ns st0))
      case concurrentlyAdded of
        Nothing -> run s
        Just leader -> clearSpace s >> run leader

-- | Only create a space (no registration).
initRuntimeSpace :: Runtime -> Namespace -> IO Space
initRuntimeSpace rt ns =
  initSpace
    (namespaceCounters ns $ counters rt)
    (httpManager rt)
    (contramap (\x -> (ns,x)) (tracer rt))
