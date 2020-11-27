{-# LANGUAGE FlexibleContexts #-}

module Prod.App
  ( app,
    appWithContext,
    initialize,
    Init,
    Runtime (..),
    alwaysReadyRuntime,
  )
where

import Data.Proxy (Proxy (..))
import Data.Aeson (ToJSON)
import Prod.Health
import Prod.Prometheus
import Prod.Status
import Servant
import Servant.Server

-- | Run a full API, with raw-serving.
type AppApi status api =
  HealthApi
    :<|> StatusApi status
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
app :: (HasServer api '[]
  , ToJSON status)
  => Init
  -> IO status
  -> Server api
  -> Proxy api
  -> Application
app (Init runtime) getStatus appHandler proxy0 =
  serve
    (proxy proxy0)
    ( handleHealth runtime
        :<|> handleStatus runtime getStatus
        :<|> handlePrometheus
        :<|> appHandler
        :<|> serveDirectoryFileServer "www"
    )
  where
    proxy :: Proxy y -> Proxy (AppApi status y)
    proxy _ = Proxy

-- | Application.
appWithContext ::
  ( HasServer api context, HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  , ToJSON status )
  =>
  Init ->
  IO status ->
  Server api ->
  Proxy api ->
  Context context ->
  Application
appWithContext (Init runtime) getStatus appHandler proxy0 context =
  serveWithContext
    (proxy proxy0)
    context
    ( handleHealth runtime
        :<|> handleStatus runtime getStatus
        :<|> handlePrometheus
        :<|> appHandler
        :<|> serveDirectoryFileServer "www"
    )
  where
    proxy :: Proxy x -> Proxy (AppApi status x)
    proxy _ = Proxy
