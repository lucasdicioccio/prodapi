# PRODAPI - AI Assistant Context

> **Bootstrap File**: This document initializes the context for AI assistants working on the prodapi project.
> If you're reading this, the documentation system has been successfully initialized.

## Project Overview

**PRODAPI** is a curated Haskell framework for building production-grade services. It provides opinionated components and patterns for:
- Web API development with Servant
- Health checking and status pages
- Metrics (Prometheus)
- Background task management
- User authentication
- Database interactions (PostgreSQL)
- Reverse proxying

### Guiding Principles

1. Developers should be aware of monitoring and operability concerns
2. The bar to entry should be low
3. Favor direct code rather than indirections
4. Use advanced type-system features tactically

## Architecture

### Component Model

Applications are built from **components** - long-running services with:
- **Handlers**: Web API handlers implementing business rules
- **Background Values**: Shared state updated asynchronously
- **Traces**: Contravariant logging for events
- **Counters**: Prometheus metrics for monitoring
- **Status Pages**: HTML/JSON status for operators
- **Healthiness**: Liveness/readiness conditions

```
┌─────────────────────────────────────────────────────────────┐
│                        COMPONENT                            │
│  ┌─────────────┐  ┌──────────────┐  ┌──────────────────┐  │
│  │  Handlers   │  │ Background   │  │     Tracers      │  │
│  │  (Servant)  │  │ Values       │  │ (Contravariant)  │  │
│  └─────────────┘  └──────────────┘  └──────────────────┘  │
│  ┌─────────────┐  ┌──────────────┐  ┌──────────────────┐  │
│  │  Counters   │  │   Status     │  │    Health        │  │
│  │(Prometheus) │  │   Pages      │  │   Checks         │  │
│  └─────────────┘  └──────────────┘  └──────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Package Structure

| Package | Purpose | Key Modules |
|---------|---------|-------------|
| `prodapi-core` | Core utilities (no Servant/Prometheus deps) | `Prod.Tracer`, `Prod.Background`, `Prod.Stepper` |
| `prodapi` | Main library with all features | `Prod.Health`, `Prod.Status`, `Prod.Prometheus` |
| `prodapi-web` | Web-specific components | `Prod.App`, `Prod.Discovery`, `Prod.Watchdog` |
| `prodapi-pg` | PostgreSQL support | `Prod.Pg.DatabaseUtils`, `Prod.Pg.TaskQueue` |
| `prodapi-userauth` | Authentication | `Prod.UserAuth`, `Prod.UserAuth.JWT` |
| `prodapi-proxy` | Reverse proxy | `Prod.Proxy`, `Prod.Proxy.MultiApp` |
| `prodapi-gen` | Documentation generators | `Prod.Gen.Docs.*` |
| `example` | Demo monitoring application | `Monitors.*`, `Hello` |

## Key Concepts

### 1. Tracer (Contravariant Logging)

```haskell
newtype Tracer m a = Tracer { runTracer :: (a -> m ()) }
```

- Contravariant functor for flexible logging
- Compose with `contramap`, `divide`, `choose`
- No upfront choice of logging backend

**Usage Pattern**:
```haskell
data MyTrace = Started | Completed Result

myFunction :: Tracer IO MyTrace -> IO ()
myFunction tracer = do
    runTracer tracer Started
    result <- operation
    runTracer tracer (Completed result)
```

### 2. Background Values

Asynchronously updated shared state:

```haskell
data BackgroundVal a = BackgroundVal
    { transform :: r -> a
    , currentValue :: IORef r
    , backgroundTask :: Async ()
    , tracer :: Tracer IO (Track r)
    }
```

**Usage Pattern**:
```haskell
-- Periodic updates
backgroundLoop tracer initialValue fetchData microseconds

-- State-passing updates  
background tracer initState defaultValue $ \state -> do
    (newVal, newState) <- compute state
    pure (newVal, newState)
```

### 3. Health Runtime

Liveness and readiness probes:

```haskell
data Runtime = Runtime
    { liveness :: IO Liveness    -- Always Alive or dead
    , readiness :: IO Readiness  -- Ready | Ill (Set Reason)
    , conditions :: IORef (Set Reason)
    , tracer :: Tracer IO Track
    }

-- Afflict/Cure pattern for dynamic readiness
afflict runtime (Reason "database_down")
cure runtime (Reason "database_down")
```

### 4. Stepper (State Machines)

Lightweight state machines for resumable execution:

```haskell
type StepIO a b = (Delayable b -> IO ()) -> Delayable a -> IO ()

data Delayable a = Inline a | Delay DelaySpec a
data DelaySpec = DelaySafetyAmount | DelayUntil UTCTime
```

Used in `prodapi-pg` for priority-based task queues.

## File Organization Convention

```
MyService/
├── Base.hs         -- Core types, pure domain logic
├── Counters.hs     -- Prometheus counters
├── Background.hs   -- Background jobs, watchdogs
├── Trace.hs        -- Component tracing types
├── Runtime.hs      -- Runtime params, connections
├── Api.hs          -- Servant API definitions
├── Handlers.hs     -- Handler implementations
└── MyService.hs    -- Re-exports for convenience
```

## Documentation Index

| Document | Description |
|----------|-------------|
| `README.md` | Project overview, principles |
| `gen/docs/docs-echo.md` | Echo endpoint for testing |
| `gen/docs/docs-health.md` | Health check API |
| `gen/docs/docs-status.md` | Status page customization |
| `gen/docs/docs-prometheus.md` | Metrics exposure |
| `gen/docs/docs-reports.md` | Client reporting API |
| `gen/docs/docs-user-auth.md` | Authentication system |
| `docs/prodapi-components.png` | Component diagram |
| `docs/prodapi-composition.png` | Composition diagram |

## Module-to-Package Mapping

```
Prod.Tracer          -> prodapi-core, prodapi
Prod.Background      -> prodapi-core, prodapi
Prod.Stepper         -> prodapi-core, prodapi

Prod.Health          -> prodapi, prodapi-web
Prod.Status          -> prodapi, prodapi-web
Prod.Prometheus      -> prodapi, prodapi-web
Prod.Reports         -> prodapi, prodapi-web
Prod.Echo            -> prodapi, prodapi-web
Prod.Watchdog        -> prodapi, prodapi-web
Prod.Discovery       -> prodapi, prodapi-web
Prod.App             -> prodapi, prodapi-web

Prod.UserAuth.*      -> prodapi-userauth
Prod.Proxy.*         -> prodapi-proxy
Prod.Pg.*            -> prodapi-pg
Prod.Gen.Docs.*      -> prodapi-gen
```

## Common Tasks

### Adding a New Component

1. Define trace types in `Trace.hs`
2. Define counters in `Counters.hs`
3. Create runtime in `Runtime.hs`
4. Implement handlers in `Handlers.hs`
5. Wire up in application main

### Creating a Status Page

```haskell
import Prod.Status

myStatusPage :: RenderStatus MyAppState
myStatusPage = defaultStatusPage $ \appState -> do
    h2_ "Custom Section"
    p_ (toHtml $ show $ someMetric appState)

-- Combine with metrics
fullStatusPage :: RenderStatus MyAppState
fullStatusPage st = do
    defaultStatusPage renderApp st
    metricsSection "/metrics.js"
```

### Setting Up Health Checks

```haskell
runtime <- alwaysReadyRuntime tracer
let runtime' = runtime 
    { readiness = checkDatabase conn <> checkCache cache
    }
```

## Dependencies to Know

| Library | Purpose |
|---------|---------|
| `servant` / `servant-server` | API definition and routing |
| `prometheus-client` | Metrics exposition |
| `lucid` | HTML generation for status pages |
| `contravariant` | Logging abstractions |
| `async` | Background task management |
| `aeson` | JSON handling |

## Critical Types Reference

```haskell
-- Tracing
Tracer m a                    -- Contravariant logger
runTracer :: Tracer m a -> a -> m ()

-- Background
BackgroundVal a               -- Async-updated value
readBackgroundVal :: MonadIO m => BackgroundVal a -> m a

-- Health
Runtime                       -- Health check runtime
Liveness                      -- Alive
Readiness                     -- Ready | Ill (Set Reason)
Reason                        -- Text reason for illness

-- Status
Status a                      -- Status page data
Identification                -- UUID for instance
RenderStatus a                -- Status -> Html ()

-- Stepper
StepIO a b                    -- Delayable state transitions
Delayable a                   -- Inline a | Delay DelaySpec a
```

## Development Tools

The project includes helper scripts in `tools/`:
- `tools/dev/read/` - File reading utilities
- `tools/dev/write/` - File writing utilities
- `tools/gen/graphviz/` - Diagram generation
- `tools/git/` - Git helpers
- `tools/web/` - Web utilities

## Build Commands

```bash
# Build all packages
cabal build all

# Build specific package
cabal build prodapi
cabal build prodapi-core
cabal build example

# Run example
cabal run example
```

---

*Last Updated*: Bootstrap initialization
*Project*: prodapi - Haskell Production API Framework
*Author*: Lucas DiCioccio

