# Component Development Guide

This guide walks through creating a complete PRODAPI component from scratch.

## What is a Component?

A component is a cohesive unit of functionality with:
- A specific purpose (e.g., user management, monitoring, proxying)
- Internal state (background values, connections)
- External interface (HTTP handlers)
- Observability (traces, counters, status, health)

## Step-by-Step Component Creation

### Step 1: Project Structure

Create a directory structure:

```
MyComponent/
├── MyComponent.cabal
├── src/
│   ├── MyComponent/
│   │   ├── Base.hs
│   │   ├── Trace.hs
│   │   ├── Counters.hs
│   │   ├── Runtime.hs
│   │   ├── Api.hs
│   │   └── Handlers.hs
│   └── MyComponent.hs
└── test/
    └── Spec.hs
```

### Step 2: Define Base Types (Base.hs)

```haskell
{-# LANGUAGE DeriveGeneric #-}
module MyComponent.Base where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Core domain types
data Widget = Widget
    { widgetId :: WidgetId
    , widgetName :: Text
    , widgetData :: WidgetData
    } deriving (Show, Generic)

newtype WidgetId = WidgetId Int
    deriving (Show, Generic, Eq, Ord)

newtype WidgetData = WidgetData Text
    deriving (Show, Generic, Eq)

-- JSON instances
instance ToJSON Widget
instance FromJSON Widget
instance ToJSON WidgetId
instance FromJSON WidgetId
instance ToJSON WidgetData
instance FromJSON WidgetData

-- Pure domain logic
createWidget :: Text -> WidgetData -> WidgetId -> Widget
createWidget name data' wid = Widget wid name data'

validateWidget :: Widget -> Either Text ()
validateWidget w
    | widgetName w == "" = Left "Widget name cannot be empty"
    | otherwise = Right ()
```

### Step 3: Define Traces (Trace.hs)

```haskell
{-# LANGUAGE DeriveGeneric #-}
module MyComponent.Trace where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import MyComponent.Base
import Prod.Tracer

-- All traceable events
data WidgetTrace
    = WidgetCreated WidgetId
    | WidgetUpdated WidgetId
    | WidgetDeleted WidgetId
    | WidgetFetchFailed WidgetId Text
    deriving (Show, Generic)

instance ToJSON WidgetTrace
instance FromJSON WidgetTrace

-- Utility to create a component tracer
widgetTracer :: Tracer IO WidgetTrace -> Tracer IO String
widgetTracer = contramap show
```

### Step 4: Define Counters (Counters.hs)

```haskell
module MyComponent.Counters where

import Prometheus (Counter, Vector, createCounter, createVector, incCounter)
import MyComponent.Base

data WidgetCounters = WidgetCounters
    { widgetCreations :: Vector Counter Text
    , widgetUpdates :: Counter
    , widgetDeletions :: Counter
    , widgetFetchFailures :: Counter
    }

registerWidgetCounters :: IO WidgetCounters
registerWidgetCounters = WidgetCounters
    <$> createVector
        (Info "widget_creations_total" "Total widget creations by type")
        ["type"]
    <*> createCounter
        (Info "widget_updates_total" "Total widget updates")
    <*> createCounter
        (Info "widget_deletions_total" "Total widget deletions")
    <*> createCounter
        (Info "widget_fetch_failures_total" "Total fetch failures")

recordCreation :: WidgetCounters -> Text -> IO ()
recordCreation counters wtype = 
    withLabel (widgetCreations counters) [wtype] incCounter

recordUpdate :: WidgetCounters -> IO ()
recordUpdate = incCounter . widgetUpdates

recordDeletion :: WidgetCounters -> IO ()
recordDeletion = incCounter . widgetDeletions

recordFetchFailure :: WidgetCounters -> IO ()
recordFetchFailure = incCounter . widgetFetchFailures
```

### Step 5: Create Runtime (Runtime.hs)

```haskell
module MyComponent.Runtime where

import Control.Concurrent.STM (TVar, newTVarIO)
import Prod.Tracer
import MyComponent.Base
import MyComponent.Trace
import MyComponent.Counters

data Runtime = Runtime
    { widgetStore :: TVar [Widget]
    , widgetTracer :: Tracer IO WidgetTrace
    , widgetCounters :: WidgetCounters
    , maxWidgets :: Int
    }

-- Initialize the runtime
initRuntime :: Tracer IO WidgetTrace -> WidgetCounters -> Int -> IO Runtime
initRuntime tracer counters maxW = do
    store <- newTVarIO []
    pure $ Runtime store tracer counters maxW

-- Runtime operations with tracing and metrics
createWidgetInStore :: Runtime -> Text -> WidgetData -> IO (Either Text Widget)
createWidgetInStore runtime name data' = do
    widgets <- readTVarIO (widgetStore runtime)
    if length widgets >= maxWidgets runtime
        then pure $ Left "Maximum widget count reached"
        else do
            let wid = WidgetId (length widgets + 1)
                widget = createWidget name data' wid
            
            -- Trace the operation
            runTracer (widgetTracer runtime) (WidgetCreated wid)
            
            -- Update metrics
            recordCreation (widgetCounters runtime) "standard"
            
            -- Store the widget
            atomically $ modifyTVar' (widgetStore runtime) (widget :)
            
            pure $ Right widget
```

### Step 6: Define API (Api.hs)

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module MyComponent.Api where

import Servant
import MyComponent.Base

-- API type
type WidgetApi =
    "widgets" :> Get '[JSON] [Widget]
    :<|> "widgets" :> Capture "id" WidgetId :> Get '[JSON] Widget
    :<|> "widgets" :> ReqBody '[JSON] CreateRequest :> Post '[JSON] Widget
    :<|> "widgets" :> Capture "id" WidgetId :> Delete '[JSON] ()

data CreateRequest = CreateRequest
    { reqName :: Text
    , reqData :: WidgetData
    } deriving (Generic)

instance FromJSON CreateRequest
instance ToJSON CreateRequest

-- Full API including status
type MyComponentApi = WidgetApi :<|> StatusApi [Widget]
```

### Step 7: Implement Handlers (Handlers.hs)

```haskell
{-# LANGUAGE DataKinds #-}
module MyComponent.Handlers where

import Control.Monad.IO.Class (liftIO)
import Servant
import Prod.Status
import Prod.Health
import MyComponent.Api
import MyComponent.Base
import MyComponent.Runtime
import MyComponent.Counters

-- Full server
widgetServer :: Runtime -> Health.Runtime -> Server MyComponentApi
widgetServer rt healthRt = 
    widgetHandlers rt healthRt :<|> statusHandler rt healthRt

-- Widget-specific handlers
widgetHandlers :: Runtime -> Health.Runtime -> Server WidgetApi
widgetHandlers rt healthRt = 
    listWidgets rt
    :<|> getWidget rt
    :<|> createWidgetHandler rt healthRt
    :<|> deleteWidget rt

listWidgets :: Runtime -> Handler [Widget]
listWidgets rt = liftIO $ readTVarIO (widgetStore rt)

getWidget :: Runtime -> WidgetId -> Handler Widget
getWidget rt wid = do
    widgets <- liftIO $ readTVarIO (widgetStore rt)
    case find (\w -> widgetId w == wid) widgets of
        Just w -> pure w
        Nothing -> throwError err404

createWidgetHandler :: Runtime -> Health.Runtime -> CreateRequest -> Handler Widget
createWidgetHandler rt healthRt req = do
    result <- liftIO $ createWidgetInStore rt (reqName req) (reqData req)
    case result of
        Right widget -> pure widget
        Left err -> do
            -- Afflict health if we're at capacity
            when (err == "Maximum widget count reached") $
                liftIO $ Health.afflict healthRt (Health.Reason "widget_capacity_reached")
            throwError $ err400 { errBody = encodeUtf8 err }

deleteWidget :: Runtime -> WidgetId -> Handler ()
deleteWidget rt wid = liftIO $ do
    atomically $ modifyTVar' (widgetStore rt) (filter (\w -> widgetId w /= wid))
    runTracer (widgetTracer rt) (WidgetDeleted wid)
    recordDeletion (widgetCounters rt)

-- Status handler
statusHandler :: Runtime -> Health.Runtime -> Handler (Status [Widget])
statusHandler rt healthRt = do
    widgets <- liftIO $ listWidgets rt
    handleStatus healthRt (pure widgets) widgetStatusPage

widgetStatusPage :: RenderStatus [Widget]
widgetStatusPage = defaultStatusPage $ \widgets -> do
    h2_ "Widgets"
    p_ $ toHtml $ "Total widgets: " <> show (length widgets)
    ul_ $ mapM_ (li_ . toHtml . widgetName) widgets
```

### Step 8: Create Main Module (MyComponent.hs)

```haskell
module MyComponent
    ( -- Re-exports for convenience
      module MyComponent.Base
    , module MyComponent.Api
    , module MyComponent.Runtime
    , module MyComponent.Trace
    , module MyComponent.Counters
    , module MyComponent.Handlers
    , WidgetTrace(..)
    ) where

import MyComponent.Api
import MyComponent.Base
import MyComponent.Counters
import MyComponent.Handlers
import MyComponent.Runtime
import MyComponent.Trace
```

### Step 9: Create Cabal File

```cabal
cabal-version:       >=1.10
name:                mycomponent
version:             0.1.0.0
synopsis:            Widget management component
build-type:          Simple

library
  exposed-modules:
      MyComponent
      MyComponent.Api
      MyComponent.Base
      MyComponent.Counters
      MyComponent.Handlers
      MyComponent.Runtime
      MyComponent.Trace
  hs-source-dirs: src
  default-extensions: 
    OverloadedStrings 
    DataKinds 
    TypeApplications 
    TypeOperators
  build-depends:
    base >= 4.7 && <5,
    prodapi,
    servant,
    servant-server,
    prometheus-client,
    stm,
    text,
    aeson,
    lucid,
    contravariant
  default-language: Haskell2010
```

### Step 10: Wire in Application

```haskell
-- app/Main.hs
module Main where

import Network.Wai.Handler.Warp (run)
import Prod.Health
import Prod.Prometheus
import Prod.Tracer
import Servant
import MyComponent

main :: IO ()
main = do
    -- Initialize infrastructure
    (promTracer, counters, registry) <- prometheusCounters
    healthTracer <- tracePrint
    
    -- Initialize health runtime
    healthRt <- alwaysReadyRuntime healthTracer
    
    -- Initialize component
    widgetCounters <- registerWidgetCounters
    componentTracer <- pure $ traceBoth promTracer (contramap show tracePrint)
    widgetRt <- initRuntime componentTracer widgetCounters 100
    
    -- Create combined tracer
    let tracer = widgetTracer widgetRt
    
    -- Start server
    putStrLn "Starting server on port 8080"
    run 8080 $ serve myComponentApi $ widgetServer widgetRt healthRt
  where
    myComponentApi = Proxy :: Proxy MyComponentApi
```

## Testing Your Component

### Unit Tests

```haskell
-- test/Spec.hs
module Main where

import Test.Hspec
import MyComponent.Base

main :: IO ()
hspec $ do
    describe "Widget validation" $ do
        it "rejects empty names" $ do
            let widget = createWidget "" (WidgetData "data") (WidgetId 1)
            validateWidget widget `shouldBe` Left "Widget name cannot be empty"
        
        it "accepts valid widgets" $ do
            let widget = createWidget "Test" (WidgetData "data") (WidgetId 1)
            validateWidget widget `shouldBe` Right ()
```

### Integration Tests

```haskell
-- Integration test with full runtime
spec :: Spec
spec = around withRuntime $ do
    describe "Widget creation" $ do
        it "creates widgets" $ \rt -> do
            result <- createWidgetInStore rt "Test" (WidgetData "data")
            isRight result `shouldBe` True
```

## Best Practices

### 1. Keep Handlers Thin

Handlers should orchestrate, not implement business logic:

```haskell
-- Good: Handler calls runtime function
handler rt = do
    result <- liftIO $ businessLogic rt input
    pure result

-- Bad: Business logic in handler
handler rt = do
    widgets <- liftIO $ readTVarIO (widgetStore rt)
    let processed = complexTransform widgets
    let validated = validateAll processed
    -- ... more logic ...
    pure result
```

### 2. Trace at Boundaries

Trace when entering/exiting your component:

```haskell
handler rt input = do
    liftIO $ runTracer (tracer rt) (HandlerStarted input)
    result <- businessLogic rt input
    liftIO $ runTracer (tracer rt) (HandlerCompleted result)
    pure result
```

### 3. Use Background Values for Shared State

```haskell
-- Good: BackgroundVal for cache
cache <- backgroundLoop tracer mempty fetchData 60000000

-- Bad: Creating new MVar/TVar per request
```

### 4. Compose Health Checks

```haskell
let runtime' = runtime 
    { readiness = mconcat
        [ checkDatabase db
        , checkCache cache
        , checkExternalService client
        ]
    }
```

### 5. Status Pages Should Be Useful

Include actionable information:

```haskell
statusPage = defaultStatusPage $ \state -> do
    h2_ "Operations"
    p_ $ toHtml $ "Queue depth: " <> show (queueDepth state)
    when (queueDepth state > 100) $ do
        p_ [style_ "color: red"] "Warning: Queue backing up!"
```

## Common Pitfalls

1. **Don't block in background tasks** - Use delays, not polling loops
2. **Don't create circular dependencies** - Components should have clear hierarchy
3. **Don't forget to link background values** - Use `link` for crash propagation
4. **Don't expose internal state directly** - Use transformations on BackgroundVal
5. **Don't ignore tracer contravariance** - Use `contramap`, not wrapper functions

## Next Steps

- See `example/` for a complete working component
- See `prodapi-userauth/` for a complex real-world component
- See `docs/architecture.md` for system-level design

