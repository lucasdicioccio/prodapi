# PRODAPI Quickstart Guide

Get up and running with PRODAPI in 5 minutes.

## Installation

### Prerequisites

- GHC 9.8.2 or later
- Cabal 3.10 or later
- PostgreSQL (optional, for prodapi-pg)

### Clone and Build

```bash
# Clone the repository
git clone <repository-url>
cd prodapi

# Build all packages
cabal build all

# Run the example application
cabal run example
```

## Your First Service

Create a minimal health-checking service:

```haskell
-- Main.hs
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Prod.Health
import Prod.Status
import Prod.Tracer
import Servant

-- Our API is just the status endpoint
type API = StatusApi ()

-- Simple status - no app-specific state
type instance AppStatus () = ()

main :: IO ()
main = do
    -- Create a simple tracer
    let tracer = tracePrint
    
    -- Create health runtime (always ready)
    healthRt <- alwaysReadyRuntime tracer
    
    -- Status page renderer
    let renderStatus = statusPage :: RenderStatus ()
    
    -- Start server
    putStrLn "Server running on http://localhost:8080"
    putStrLn "Try: curl http://localhost:8080/status"
    run 8080 $ serve (Proxy :: Proxy API) $ 
        handleStatus healthRt (pure ()) renderStatus
```

```cabal
-- quickstart.cabal
cabal-version:       >=1.10
name:                quickstart
version:             0.1.0.0

executable quickstart
  main-is:             Main.hs
  build-depends:       base >=4.13
                     , prodapi
                     , servant-server
                     , wai
                     , warp
  default-language:    Haskell2010
```

Run it:

```bash
cabal run quickstart
# In another terminal:
curl http://localhost:8080/status
```

## Adding a Custom Endpoint

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Prod.Health
import Prod.Status
import Prod.Tracer
import Servant
import Data.Time (getCurrentTime)

type API = "time" :> Get '[JSON] String :<|> StatusApi String

timeHandler :: Handler String
timeHandler = show <$> liftIO getCurrentTime

main :: IO ()
main = do
    healthRt <- alwaysReadyRuntime tracePrint
    
    let server = timeHandler :<|> statusHandler healthRt
        statusHandler rt = handleStatus rt getStatus statusPage
        getStatus = show <$> getCurrentTime
    
    run 8080 $ serve (Proxy :: Proxy API) server
```

## Adding Background Updates

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prod.Background
import Prod.Health
import Prod.Status
import Prod.Tracer
import Servant
import Network.Wai.Handler.Warp (run)
import Data.IORef

type API = StatusApi Int

main :: IO ()
main = do
    -- Create a background counter that increments every second
    counter <- backgroundLoop tracePrint 0 (pure . (+1)) 1000000
    
    -- Health runtime
    healthRt <- alwaysReadyRuntime tracePrint
    
    -- Server that shows current counter value
    let server = handleStatus healthRt 
            (readBackgroundVal counter) 
            (defaultStatusPage $ \n -> p_ (toHtml $ "Counter: " ++ show n))
    
    putStrLn "Server running on http://localhost:8080"
    run 8080 $ serve (Proxy :: Proxy API) server
```

## Adding Health Checks

```haskell
main :: IO ()
main = do
    -- Create a database connection pool
    dbPool <- createPool ...
    
    -- Check if DB is healthy
    let checkDB = do
            conn <- try $ takeResource dbPool
            case conn of
                Left (_ :: SomeException) -> pure $ Ill (Set.singleton (Reason "db_unavailable"))
                Right _ -> pure Ready
    
    -- Health runtime with DB check
    baseRt <- alwaysReadyRuntime tracePrint
    let healthRt = baseRt { readiness = checkDB }
    
    -- Rest of your app...
```

## Adding Metrics

```haskell
import Prod.Prometheus
import Prometheus (incCounter)

main :: IO ()
main = do
    -- Create prometheus counters
    (promTracer, counters, registry) <- prometheusCounters
    
    -- Create a request counter
    requestCounter <- createCounter (Info "requests_total" "Total requests")
    
    -- Use in handler
    let handler = do
            liftIO $ incCounter requestCounter
            pure "Hello!"
    
    -- Serve metrics at /metrics
    -- ...
```

## Adding Authentication

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prod.UserAuth
import Prod.UserAuth.HandlerCombinators
import Servant

-- Protected API
type ProtectedAPI = AuthRequired :> "secret" :> Get '[JSON] String

type API = AuthApi :<|> ProtectedAPI

server :: Server API
server = authServer :<|> secretHandler
  where
    secretHandler (Authenticated user) = pure $ "Hello, " ++ userName user
    secretHandler _ = throwError err401

main :: IO ()
main = do
    -- Initialize auth runtime
    authRt <- initAuthRuntime ...
    
    -- Run server
    run 8080 $ serveWithContext (Proxy :: Proxy API) 
        (authContext authRt) 
        server
```

## Project Template

Use this structure for new services:

```
my-service/
├── my-service.cabal
├── src/
│   ├── MyService/
│   │   ├── Base.hs       -- Types and pure functions
│   │   ├── Trace.hs      -- Tracing events
│   │   ├── Counters.hs   -- Prometheus metrics
│   │   ├── Runtime.hs    -- Component runtime
│   │   ├── Api.hs        -- Servant API types
│   │   └── Handlers.hs   -- Handler implementations
│   └── MyService.hs      -- Re-exports
├── app/
│   └── Main.hs           -- Application entry point
└── test/
    └── Spec.hs           -- Tests
```

## Common Commands

```bash
# Build
cabal build

# Run
cabal run

# REPL with all packages
cabal repl all

# Run specific test suite
cabal test my-service

# Build with optimizations
cabal build -O2

# Build specific package
cabal build prodapi-core
cabal build prodapi-pg

# Watch for changes and rebuild
cabal build --enable-tests --file-watch
```

## Debugging Tips

### Enable Tracing

```haskell
-- Use tracePrint for development
let tracer = tracePrint

-- Or trace to a file
withFile "/var/log/myapp.log" AppendMode $ \h -> do
    let tracer = traceHPrint h
    -- ...
```

### Check Health Status

```bash
# Liveness probe
curl http://localhost:8080/health/alive

# Readiness probe
curl http://localhost:8080/health/ready

# Drain (mark as not ready)
curl -X POST http://localhost:8080/health/drain
```

### View Status Page

Open http://localhost:8080/status in your browser for a visual status page.

### View Metrics

```bash
# Raw Prometheus format
curl http://localhost:8080/metrics
```

## Next Steps

1. **Read the full docs**:
   - `docs/architecture.md` - System design
   - `docs/component-guide.md` - Building components
   - `docs/module-index.md` - Module reference

2. **Explore the example**:
   ```bash
   cd example
   cabal run
   ```

3. **Study real components**:
   - `prodapi-userauth/` - Complex authentication component
   - `prodapi-pg/` - Database and task queue component
   - `prodapi-proxy/` - Reverse proxy component

4. **Join the community**:
   - File issues on GitHub
   - Submit PRs for improvements
   - Ask questions in discussions

## Troubleshooting

### Compilation Errors

**"Could not find module Prod.X"**
- Make sure you've added `prodapi` to your build-depends
- Check that you're using the right package (prodapi vs prodapi-web)

**"Servant type errors"**
- Make sure you have `DataKinds` and `TypeOperators` enabled
- Check that API types end with `:<|> StatusApi`

### Runtime Errors

**"Thread blocked indefinitely in STM transaction"**
- Make sure you're using `atomically` for STM operations
- Check for deadlocks in background tasks

**"Health check failing"**
- Use `curl http://localhost:8080/health/ready` to see why
- Check that all conditions are being cured properly

**"Memory leak"**
- Make sure to `kill` background values when done
- Check that tracers aren't accumulating state

## Getting Help

- Check `docs/` for detailed documentation
- Look at `example/` for working code
- Read the generated API docs in `gen/docs/`
- File an issue with error messages and reproduction steps

