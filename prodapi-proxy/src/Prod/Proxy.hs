-- TODO:
-- - more counters (on failure)
-- - tracer
module Prod.Proxy where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Ord (comparing)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.HTTP.Client as HTTP (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (status404)
import Network.HTTP.ReverseProxy
import qualified Network.Wai as Wai
import qualified Prometheus as Prometheus
import Servant
import Servant.Server

import Prod.Health (Readiness(..))
import Prod.Discovery (Discovery)
import qualified Prod.Discovery as Discovery
import Prod.Background (BackgroundVal)
import qualified Prod.Background as BackgroundVal

type Api = ProxyRequestApi

type ProxyRequestApi = Raw

data Counters = Counters
  { cnt_proxied_requests :: Prometheus.Counter
  }

initCounters :: IO Counters
initCounters =
  Counters
    <$> Prometheus.register (Prometheus.counter (Prometheus.Info "cnt_proxied_requests" "number of requests proxied"))

type Host = ByteString
type Port = Int

type LookupHostPort = Wai.Request -> IO (Maybe (Host, Port))

-- | Helpers to build backends WaiProxyResponse.
data Backends
  = StaticBackend Host Port
  | DynamicBackend LookupHostPort
  | WaiProxyBackend (Wai.Request -> IO WaiProxyResponse)

firstBackend :: IO [(Host, Port)] -> LookupHostPort
firstBackend disc = const f
  where
    f :: IO (Maybe (Host, Port))
    f = safeHead <$> disc

data Runtime = Runtime {
    counters :: Counters
  , backends :: Backends
  , httpManager :: HTTP.Manager
  }

initRuntime :: Backends -> IO Runtime
initRuntime backends =
  Runtime
    <$> initCounters
    <*> pure backends
    <*> newManager defaultManagerSettings

handle :: Runtime -> Server Api
handle rt = coerce (handleProxy rt)

handleProxy :: Runtime -> Application
handleProxy rt =
  case backends rt of
    StaticBackend host port ->
      let
        destination = const $ pure $ WPRProxyDest $ ProxyDest host port
      in
      \req rsp -> do
        countProxiedQuery
        waiProxyTo destination defaultOnExc (httpManager rt) req rsp
    DynamicBackend lookup ->
      \req rsp -> do
        countProxiedQuery
        dest <- lookup req
        case dest of
          Just (host, port) -> do
            let destination = const $ pure $ WPRProxyDest $ ProxyDest host port
            waiProxyTo destination defaultOnExc (httpManager rt) req rsp
          Nothing ->
            rsp $ Wai.responseLBS status404 [] "api proxy runtime is disabled"
    WaiProxyBackend lookup ->
      \req rsp -> do
        countProxiedQuery
        waiProxyTo lookup defaultOnExc (httpManager rt) req rsp
  where
    countProxiedQuery :: IO ()
    countProxiedQuery = liftIO $ Prometheus.incCounter $ cnt_proxied_requests $ counters rt

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

