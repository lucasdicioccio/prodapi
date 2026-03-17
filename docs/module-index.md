# PRODAPI Module Index

## prodapi-core

Core utilities with minimal dependencies (no Servant, no Prometheus).

### Prod.Tracer
**Purpose**: Contravariant logging primitives

```haskell
newtype Tracer m a = Tracer { runTracer :: (a -> m ()) }

-- Core operations
silent :: Applicative m => Tracer m a
traceBoth :: Applicative m => Tracer m a -> Tracer m a -> Tracer m a
traceIf :: Applicative m => (a -> Bool) -> Tracer m a -> Tracer m a
pulls :: Monad m => (c -> m b) -> Tracer m b -> Tracer m c

-- Utility tracers
tracePrint :: (MonadIO m, Show a) => Tracer m a
traceHPrint :: (MonadIO m, Show a) => Handle -> Tracer m a
traceHPut :: MonadIO m => Handle -> Tracer m ByteString
encodeJSON :: ToJSON a => Tracer m ByteString -> Tracer m a
```

**Used by**: All packages

---

### Prod.Background
**Purpose**: Asynchronously updated shared state

```haskell
data BackgroundVal a
background :: Tracer IO (Track a) -> b -> a -> (b -> IO (a, b)) -> IO (BackgroundVal a)
backgroundLoop :: Tracer IO (Track a) -> a -> IO a -> MicroSeconds Int -> IO (BackgroundVal a)
readBackgroundVal :: MonadIO m => BackgroundVal a -> m a
kill :: (HasCallStack, MonadIO m) => BackgroundVal a -> m ()
link :: BackgroundVal a -> BackgroundVal b -> IO ()

data Track r = Init r | RunStart | RunDone r r | Kill CallStack
```

**Used by**: prodapi, prodapi-web, example

---

### Prod.Stepper
**Purpose**: Lightweight state machines for resumable execution

```haskell
type StepIO a b = (Delayable b -> IO ()) -> Delayable a -> IO ()
type StartStepIO a b = (Delayable b -> IO ()) -> a -> IO ()

data Delayable a = Inline a | Delay DelaySpec a
data DelaySpec = DelaySafetyAmount | DelayUntil UTCTime | DelayUntilEpochInteger Int64

defineExecution :: Tracer IO (Trace z1 z2) -> (a -> z1) -> (b -> z2) 
                -> (ExecFunctions b -> a -> IO ()) -> StepIO a b

waitUntil :: SafetySeconds -> DelaySpec -> IO ()
isExpired :: DelaySpec -> IO Bool
```

**Used by**: prodapi-pg (TaskQueue)

---

## prodapi

Main library with full feature set.

### Prod.Health
**Purpose**: Liveness and readiness health checks

```haskell
data Runtime = Runtime
    { liveness :: IO Liveness
    , readiness :: IO Readiness
    , conditions :: IORef (Set Reason)
    , tracer :: Tracer IO Track
    }

data Liveness = Alive
data Readiness = Ready | Ill (Set Reason)
newtype Reason = Reason Text

alwaysReadyRuntime :: Tracer IO Track -> IO Runtime
withReadiness :: IO Readiness -> Runtime -> Runtime
withLiveness :: IO Liveness -> Runtime -> Runtime
completeReadiness :: Runtime -> IO Readiness

type HealthApi = GetLivenessApi :<|> GetReadinessApi :<|> DrainApi
handleHealth :: Runtime -> Server HealthApi
```

**Related**: See `gen/docs/docs-health.md` for API details

---

### Prod.Status
**Purpose**: HTML/JSON status pages

```haskell
type StatusApi a = "status" :> Get '[HTML, JSON] (Status a)

data Status a = Status
    { identification :: Identification
    , liveness :: Liveness
    , readiness :: Readiness
    , appStatus :: a
    , renderer :: RenderStatus a
    }

type RenderStatus a = Status a -> Html ()

handleStatus :: Runtime -> IO a -> RenderStatus a -> Handler (Status a)
defaultStatusPage :: (a -> Html ()) -> RenderStatus a
statusPage :: ToHtml a => RenderStatus a
metricsSection :: MetricsJSurl -> RenderStatus a
versionsSection :: [(String, Version)] -> RenderStatus a
```

**Related**: See `gen/docs/docs-status.md` for API details

---

### Prod.Prometheus
**Purpose**: Metrics exposition

```haskell
-- Counter management
prometheusCounters :: IO (Tracer IO Counter, Counters, Registry)
prometheusRegistry :: Registry -> IO Text

-- See gen/docs/docs-prometheus.md for full API
```

**Related**: See `gen/docs/docs-prometheus.md` for API details

---

### Prod.Echo
**Purpose**: Echo endpoint for testing round-trips

```haskell
type EchoApi = ...
echoServer :: Server EchoApi
```

**Related**: See `gen/docs/docs-echo.md` for API details

---

### Prod.Reports
**Purpose**: Client log reporting

```haskell
-- Client-side log collection
-- See gen/docs/docs-reports.md for full API
```

**Related**: See `gen/docs/docs-reports.md` for API details

---

### Prod.Watchdog
**Purpose**: File monitoring background tasks

```haskell
-- Monitors files and updates counters
-- Used for detecting file changes, process health
```

---

### Prod.Discovery
**Purpose**: DNS-based service discovery

```haskell
-- Background DNS resolution
-- Updates BackgroundVal with resolved addresses
```

---

### Prod.Healthcheck
**Purpose**: Client for checking external services

```haskell
-- Stateful health check client
-- Tracks state transitions of external dependencies
```

---

### Prod.MimeTypes
**Purpose**: MIME type definitions

```haskell
data HTML  -- For servant content-type negotiation
```

---

### Prod.App
**Purpose**: Application helpers

```haskell
-- Common application patterns
-- Wiring utilities
```

---

## prodapi-web

Web-specific re-exports and conveniences.

### Re-exported from prodapi
- `Prod.App`
- `Prod.Background`
- `Prod.Discovery`
- `Prod.Echo`
- `Prod.Health`
- `Prod.Healthcheck`
- `Prod.MimeTypes`
- `Prod.Prometheus`
- `Prod.Reports`
- `Prod.Status`
- `Prod.Stepper`
- `Prod.Tracer`
- `Prod.Watchdog`

**Purpose**: Single import for web applications

---

## prodapi-pg

PostgreSQL support.

### Prod.Pg.DatabaseUtils
**Purpose**: Database connection helpers

```haskell
-- Connection pooling
-- Query tracing
-- Error handling
```

---

### Prod.Pg.TaskQueue
**Purpose**: Priority-based resumable task queue

```haskell
-- Uses Stepper for state machine execution
-- PostgreSQL-backed for durability
-- Priority ordering with delay support
```

---

## prodapi-userauth

Authentication and authorization.

### Prod.UserAuth
**Purpose**: Main authentication module

```haskell
-- Re-exports common types
```

---

### Prod.UserAuth.Base
**Purpose**: Core authentication types

```haskell
data User = ...
data Session = ...
-- JWT claims
-- Cookie handling
```

---

### Prod.UserAuth.Api
**Purpose**: Authentication API endpoints

```haskell
type AuthApi = ...
-- Login/logout endpoints
-- Registration
```

---

### Prod.UserAuth.JWT
**Purpose**: JWT token handling

```haskell
-- Token generation
-- Token validation
-- Claims extraction
```

---

### Prod.UserAuth.HandlerCombinators
**Purpose**: Servant combinators for protected endpoints

```haskell
-- Cookie-based auth protection
-- Claim requirements
-- Session validation
```

---

### Prod.UserAuth.Runtime
**Purpose**: Authentication runtime

```haskell
-- DB connections for auth
-- Token configuration
-- Cookie settings
```

---

### Prod.UserAuth.Counters
**Purpose**: Auth-specific metrics

```haskell
-- Login attempts
-- Failed authentications
-- Session counts
```

---

### Prod.UserAuth.Trace
**Purpose**: Auth tracing events

```haskell
data AuthTrace = LoginAttempt | LoginSuccess | LoginFailure ...
```

---

### Prod.UserAuth.Backend
**Purpose**: Database storage for users/sessions

```haskell
-- User CRUD
-- Session management
-- PostgreSQL specific
```

---

### Prod.UserAuth.OAuth2
**Purpose**: OAuth2 integration helpers

```haskell
-- OAuth2 flow support
-- Provider integration
```

---

## prodapi-proxy

Reverse proxying support.

### Prod.Proxy
**Purpose**: Main proxy module

```haskell
-- Re-exports and high-level API
```

---

### Prod.Proxy.Base
**Purpose**: Core proxy types

```haskell
data ProxyConfig = ...
-- Request/response handling
```

---

### Prod.Proxy.Lookups
**Purpose**: Backend discovery

```haskell
-- Dynamic backend lookup
-- Health-based routing
```

---

### Prod.Proxy.MultiApp
**Purpose**: Multi-application proxying

```haskell
-- Route to different backends by path/host
-- Application multiplexing
```

---

### Prod.Proxy.R
**Purpose**: Request manipulation

```haskell
-- Request rewriting
-- Header manipulation
```

---

### Prod.Proxy.Compat
**Purpose**: Compatibility helpers

```haskell
-- Version compatibility
-- Migration helpers
```

---

## prodapi-gen

Documentation and code generation.

### Prod.Gen.Docs.Echo
Generates `gen/docs/docs-echo.md`

### Prod.Gen.Docs.Health
Generates `gen/docs/docs-health.md`

### Prod.Gen.Docs.Status
Generates `gen/docs/docs-status.md`

### Prod.Gen.Docs.Prometheus
Generates `gen/docs/docs-prometheus.md`

### Prod.Gen.Docs.Reports
Generates `gen/docs/docs-reports.md`

### Prod.Gen.Docs.UserAuth
Generates `gen/docs/docs-user-auth.md`

---

## Example Application

### Hello
Simple component demonstrating basic patterns.

### Monitors
Full monitoring component with:
- `Monitors.Base` - Core types
- `Monitors.Counters` - Prometheus counters
- `Monitors.Background` - Watchdogs and discovery
- `Monitors.Api` - Servant API definition
- `Monitors.Handlers` - Handler implementations

### BackgroundNetwork
Network-related background tasks.

### Main
Application wiring and startup.

---

## Module Import Guide

### For a minimal service:
```haskell
import Prod.Tracer        -- Logging
import Prod.Background    -- Async state
import Prod.Health        -- Health checks
import Prod.Status        -- Status pages
```

### For a web service:
```haskell
import Prod.Web           -- Everything web-related
-- OR more specifically:
import Prod.App
import Prod.Health
import Prod.Status
import Prod.Prometheus
import Prod.Echo
```

### For authentication:
```haskell
import Prod.UserAuth
import Prod.UserAuth.HandlerCombinators  -- For protecting endpoints
```

### For database:
```haskell
import Prod.Pg.DatabaseUtils
import Prod.Pg.TaskQueue      -- If using task queue
```

### For proxying:
```haskell
import Prod.Proxy
import Prod.Proxy.MultiApp    -- For multi-tenant setup
```

---

## Dependency Graph by Module

```
Prod.Tracer
    ├── Prod.Background
    ├── Prod.Health
    ├── Prod.UserAuth.*
    └── Prod.Pg.*

Prod.Background
    ├── Prod.Watchdog
    ├── Prod.Discovery
    └── Prod.Pg.TaskQueue

Prod.Stepper
    └── Prod.Pg.TaskQueue

Prod.Health
    └── Prod.Status

Prod.Prometheus
    ├── Prod.Status
    └── Prod.UserAuth.Counters
```

