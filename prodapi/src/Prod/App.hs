{-# LANGUAGE FlexibleContexts #-}

module Prod.App
  ( app,
    appWithContext,
    initialize,
    Init,
    Runtime (..),
    defaultRuntime,
  )
where

import Data.Proxy (Proxy (..))
import Prod.Health
import Prod.Prometheus
import Prod.Status
import Servant
import Servant.Server

-- | Run a full API, with raw-serving.
type AppApi api =
  HealthApi
    :<|> StatusApi
    :<|> PrometheusApi
    :<|> api
    :<|> Raw

-- | Opaque proof of initialization.
data Init = Init Runtime

-- | Initializes internal data.
initialize :: Runtime -> IO Init
initialize runtime =
  initPrometheus >> pure (Init runtime)

-- | Application.
app :: HasServer api '[]
  => Init
  -> Server api
  -> Proxy api
  -> Application
app (Init runtime) appHandler proxy0 =
  serve
    (proxy proxy0)
    ( handleHealth runtime
        :<|> handleStatus runtime
        :<|> handlePrometheus
        :<|> appHandler
        :<|> serveDirectoryFileServer "www"
    )
  where
    proxy :: Proxy x -> Proxy (AppApi x)
    proxy _ = Proxy

-- | Application.
appWithContext ::
  (HasServer api context, HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters)
  =>
  Init ->
  Server api ->
  Proxy api ->
  Context context ->
  Application
appWithContext (Init runtime) appHandler proxy0 context =
  serveWithContext
    (proxy proxy0)
    context
    ( handleHealth runtime
        :<|> handleStatus runtime
        :<|> handlePrometheus
        :<|> appHandler
        :<|> serveDirectoryFileServer "www"
    )
  where
    proxy :: Proxy x -> Proxy (AppApi x)
    proxy _ = Proxy
