{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger as RequestLogger

import Control.Applicative ((<|>))
import Control.Monad (when)
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Text (Text)
import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import Data.Proxy (Proxy(..))
import Servant
import Servant.Server
import qualified Prometheus as Prometheus
import Prod.App as Prod
import Prod.Status (statusPage, metricsSection, versionsSection, this)
import Prod.Health as Health
import qualified Prod.UserAuth as Auth
import qualified Prod.UserAuth.Base as Auth
import qualified Prod.Discovery as Discovery
import qualified Prod.Healthcheck as Healthcheck
import qualified Prod.Proxy as ProdProxy
import qualified Prod.Proxy.MultiApp as MultiApp
import Prod.Tracer (Tracer(..), choose, tracePrint, traceHPrint, traceHPut, encodeJSON, pulls)

import qualified Data.Text.Encoding as Text
import qualified BackgroundNetwork
import qualified Hello
import qualified Monitors
import qualified Monitors.Base as Monitors

import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import Lucid (HtmlT, ToHtml(..), h4_, div_, p_, ul_, li_, a_, href_, with, form_, id_, action_, method_, label_, for_, type_, input_, name_, value_)

import System.IO (stdout, stderr)
import qualified Paths_prodapi

-- simulate backend services
type ProxiedServiceApi = "proxied" :> ProdProxy.Api


type FullApi = Hello.Api
  :<|> ProxiedServiceApi
  :<|> Monitors.Api
  :<|> Auth.UserAuthApi Monitors.MyUserInfo

data ExampleStatus = ExampleStatus
  { registrations :: [Monitors.Registration]
  , hosts :: [Discovery.Host]
  , summaries :: Map.Map Healthcheck.Namespace Healthcheck.SummaryMap
  } deriving (Generic)
instance ToJSON ExampleStatus
instance ToHtml ExampleStatus where
  toHtml = renderStatus
  toHtmlRaw = renderStatus

renderStatus :: forall m. (Monad m) => ExampleStatus -> HtmlT m ()
renderStatus (ExampleStatus regs hosts checks) = div_ $ do
    h4_ "example status"
    p_ $ toHtml $ "registrations (" <> (show $ length regs) <> ")"
    ul_ $ do
      traverse_ (renderRegistration) regs
  
    h4_ "add ping target"
    p_ $ with form_ [ id_ "add-ping-form", action_ "/monitors/ping" , method_ "post" ] $ do
           p_ $ do
             label_ [ for_ "add-ping-host", type_ "text"  ] "host"
             input_ [ type_ "text", id_ "add-ping-host", name_ "host" ]
           p_ $ do
             input_ [ type_ "submit", value_ "add" ]

    h4_ "discovered hosts"
    ul_ $ do
      traverse_ renderHost hosts

    h4_ "healthchecked hosts"
    ul_ $ do
      traverse_ renderNamespacedChecks $ Map.toList checks
  where
    renderRegistration :: Monitors.Registration -> HtmlT m ()
    renderRegistration (Monitors.Registration reg) = li_ $ do
      with a_ [ href_ (readMonitorUrl reg) ] $ toHtml reg
      with form_ [ action_ "/monitors/ping-delete" , method_ "post" ] $ do
           p_ $ do
             input_ [ type_ "hidden", id_ "del-ping-host", name_ "registration", value_ reg ]
           p_ $ do
             input_ [ type_ "submit", value_ "del" ]

    readMonitorUrl reg = "/monitors/ping/latest?target=" <> reg

    renderHost :: Discovery.Host -> HtmlT m ()
    renderHost txt = li_ $ p_ $ toHtml txt

    renderNamespacedChecks :: (Healthcheck.Namespace, Healthcheck.SummaryMap) -> HtmlT m ()
    renderNamespacedChecks (ns,cs) =
      ul_ $ do
        p_ $ toHtml ns
        renderSummaryMap cs

    renderSummaryMap :: Healthcheck.SummaryMap -> HtmlT m ()
    renderSummaryMap cs =
      ul_ $ do
        traverse_ renderHealthCheckResult $ Map.toList cs

    renderHealthCheckResult :: ((Healthcheck.Host, Healthcheck.Port), Healthcheck.CheckSummary) -> HtmlT m ()
    renderHealthCheckResult ((h,p),s) =
      li_ $ do
        div_ $ do
          p_ $ do
            toHtml h
            toHtml (":" :: Text)
            toHtml (show p)
          p_ $ do
            renderCheckSummary s

    renderCheckSummary :: Healthcheck.CheckSummary -> HtmlT m ()
    renderCheckSummary s = do
      when (Healthcheck.healthChecked s) $ do
        p_ "healthchecked"
        when (Healthcheck.recentlyHealthy s) $ do
          p_ "healthy"
        when (Healthcheck.neverHealthy s) $ do
          p_ "never-healthy"
      when (not $ Healthcheck.healthChecked s) $ do
        p_ "insufficient healthchecks"


exampleStatus :: Hello.Runtime -> Monitors.Runtime -> IO ExampleStatus
exampleStatus hRt mRt = do
  ExampleStatus
    <$> Monitors.readRegistrations mRt
    <*> Hello.readDiscoveredHosts1 hRt
    <*> Healthcheck.readSpaces (Hello.healthchecks hRt)

hasFoundHostsReadiness :: Hello.Runtime -> IO Readiness
hasFoundHostsReadiness = fmap adapt . Hello.readDiscoveredHosts1
  where
    adapt :: [Discovery.Host] -> Readiness
    adapt [] = Ill $ Set.fromList [Reason "no hosts found"]
    adapt _  = Ready

logPrint :: Show a => Tracer IO a
logPrint = Tracer print

logUserAuth :: Tracer IO (Auth.Track Monitors.MyUserInfo)
logUserAuth = Tracer f
  where
    f (Auth.Behaviour (Auth.Attempt _ Auth.LoginFailed)) = print $ "ua: login attempt failed"
    f (Auth.Behaviour (Auth.Attempt _ _)) = print $ "ua: login attempt success"
    f (Auth.Behaviour (Auth.Verification t)) = print $ "ua: verif: " <> show t
    f (Auth.Behaviour (Auth.OptionalVerification t)) = print $ "ua: opt-verif: " <> show t
    f (Auth.Behaviour (Auth.Registration _ Auth.RegisterFailure)) = print $ "ua: user-registration failed"
    f (Auth.Behaviour (Auth.Registration _ _)) = print $ "ua: user-registration success"
    f (Auth.Behaviour (Auth.Recovery _ _)) = print $ "ua: pass-recovery requested"
    f (Auth.Behaviour (Auth.ApplyRecovery _ t)) = print $ "ua: pass-recovery applied: " <> show t
    f (Auth.Bearer (Auth.Allowed Nothing)) = print "ua: jwt-limited"
    f (Auth.Bearer (Auth.Allowed (Just True))) = print "ua: jwt-allow"
    f (Auth.Bearer (Auth.Allowed (Just False))) = print "ua: jwt-disallow"
    f (Auth.Bearer (Auth.Extracted jwt)) = print $ "ua: jwt:" <> show jwt
    f (Auth.Bearer (Auth.Signed uid claims)) = print $ "ua: jwt-signed:" <> show (uid,claims)
    f (Auth.Backend Auth.SQLConnect) = print "ua: sql conn"
    f (Auth.Backend Auth.SQLTransaction) = print "ua: sql tx"
    f (Auth.Backend Auth.SQLRollback) = print "ua: sql tx"
    f (Auth.Backend (Auth.SQLQuery bs)) = print $ "ua: sql query" <> bs
    f (Auth.Callbacks (Auth.AugmentCookie _)) = print $ "cb: cookie augmented"

logHealth :: Tracer IO Health.Track
logHealth = Tracer f
  where
    f (Health.Afflict cs r) = print $ "health: afflict " <> show r <> " from: " <> show cs
    f (Health.Cure _ r) = print $ "health: cure " <> show r

logMonitors :: Tracer IO Monitors.Track
logMonitors = choose f (pulls wrapWithThis . encodeJSON $ traceHPut stdout) (choose g (traceHPrint stderr) tracePrint)
  where
    wrapWithThis obj = pure (obj, this)
    f (Monitors.Registered r) = Left r
    f v                       = Right v
    g v@(Monitors.PingVal _ _) = Left v
    g v                        = Right v

main :: IO ()
main = do
  _ <- BackgroundNetwork.complicatedNetworkOfBackgroundUpdates tracePrint
  helloRt <- Hello.initRuntime logPrint
  let augmentSession _ _ = pure $ Just $ Monitors.MyUserInfo "demo-info"
  let augmentWhoAmI _ _ = pure $ Just $ Monitors.MyUserInfo "demo-whoami"
  let augmentCookie _ = pure $ Right [("demo-roles", Aeson.toJSON ["superuser" :: Text])]
  authRt <- Auth.initRuntime "secret-value" mempty "postgres://prodapi:prodapi@localhost:5432/prodapi_example" augmentSession augmentWhoAmI augmentCookie (logUserAuth)
  monitorsRt <- Monitors.initRuntime logMonitors authRt
  -- for demonstration purpose we initRuntime twice but there will be collisions on the metric name here
  -- most applications should either have a single ProdProxy runtime (possibly called with multiple 'ProdProxy.handle').
  service1Rt <- ProdProxy.initRuntime (ProdProxy.StaticBackend "httpbin.org" 80)
  service2Rt <- ProdProxy.initRuntime (ProdProxy.DynamicBackend $ hostPortForDiscovered helloRt (Hello.discovery1 helloRt) "dyncioccio")
  service3Rt <- ProdProxy.initRuntime (ProdProxy.DynamicBackend $ hostPortForDiscovered helloRt (Hello.discovery2 helloRt) "service3")

  healthRt <- Health.withReadiness (hasFoundHostsReadiness helloRt) <$> Prod.alwaysReadyRuntime logHealth
  init <- initialize healthRt
  let fullApp proxiedRt = appWithContext
                    init
                    (exampleStatus helloRt monitorsRt)
                    (statusPage <> versionsSection [("prodapi", Paths_prodapi.version)] <> Auth.renderStatus <> metricsSection "metrics.js")
                    (Hello.serve helloRt
                     :<|> ProdProxy.handle proxiedRt
                     :<|> Monitors.handle monitorsRt
                     :<|> Auth.handleUserAuth authRt
                    )
                    (Proxy @FullApi)
                    (Auth.authServerContext authRt)
  Warp.run
    7708
    $ RequestLogger.logStdoutDev
    $ MultiApp.routeApplication (Map.fromList [("httpbin.org",fullApp service1Rt),("dyncioccio.localhost",fullApp service2Rt)]) (fullApp service3Rt)

hostPortForDiscovered :: Hello.Runtime -> Discovery.Discovery [Text] -> Healthcheck.Namespace -> ProdProxy.LookupHostPort
hostPortForDiscovered helloRt disc ns =
    ProdProxy.toLookup ((healthy <|> ProdProxy.lookup fallback <|> ProdProxy.lookup nobackend) <* countAll)
  where
    countAll :: ProdProxy.R ()
    countAll = ProdProxy.io $ Prometheus.incCounter (Hello.routedQueries . Hello.counters $ helloRt)

    healthy :: ProdProxy.R (ProdProxy.Host, ProdProxy.Port)
    healthy = fmap ProdProxy.pickHealthy (ProdProxy.io healthcheckedBackends) >>= ProdProxy.shuffle >>= ProdProxy.safeHead

    nobackend = ProdProxy.countWith (Hello.nobackendProxiedQueries . Hello.counters $ helloRt) (ProdProxy.noBackend)

    fallback = ProdProxy.countWith (Hello.fallbackProxiedQueries . Hello.counters $ helloRt) (ProdProxy.randomBackend ioBackends)
    hcs = Hello.healthchecks helloRt
    healthcheckedBackends = Healthcheck.withSpace hcs ns Healthcheck.readBackgroundChecks
    ioBackends = adapt <$> Discovery.readCurrent disc
    adapt (Discovery.Found _ hps) = fmap toHP hps
    adapt _ = []
    toHP h = (Text.encodeUtf8 h, 80)
