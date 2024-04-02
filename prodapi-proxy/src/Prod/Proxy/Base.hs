module Prod.Proxy.Base where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client as HTTP (Manager, defaultManagerSettings, newManager)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types.Status (status404)
import qualified Network.Wai as Wai
import qualified Prometheus as Prometheus
import Servant
import Servant.Server

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

data Runtime = Runtime
    { counters :: Counters
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
