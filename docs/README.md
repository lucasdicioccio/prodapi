# PRODAPI Documentation

Welcome to the PRODAPI documentation. This directory contains comprehensive guides for understanding, using, and extending the framework.

## Documentation Index

### Getting Started

| Document | Description | Read This If... |
|----------|-------------|-----------------|
| [`quickstart.md`](quickstart.md) | 5-minute quickstart guide | You want to get something running quickly |
| [`component-guide.md`](component-guide.md) | Step-by-step component creation | You're building a new service/component |
| [Example Application](../example/) | Working example code | You want to see a complete implementation |

### Understanding PRODAPI

| Document | Description | Read This If... |
|----------|-------------|-----------------|
| [`AI-CONTEXT.md`](AI-CONTEXT.md) | AI assistant context and quick reference | You're an AI assistant working on this codebase |
| [`architecture.md`](architecture.md) | System architecture and design patterns | You want to understand how PRODAPI works |
| [`module-index.md`](module-index.md) | Complete module reference | You need to find a specific module or function |
| [Main README](../README.md) | Project overview and principles | You're new to the project |

### API Documentation

Generated API documentation is in [`gen/docs/`](../gen/docs/):

| Document | Description |
|----------|-------------|
| [`docs-echo.md`](../gen/docs/docs-echo.md) | Echo endpoint for testing |
| [`docs-health.md`](../gen/docs/docs-health.md) | Health check API |
| [`docs-status.md`](../gen/docs/docs-status.md) | Status page API |
| [`docs-prometheus.md`](../gen/docs/docs-prometheus.md) | Prometheus metrics API |
| [`docs-reports.md`](../gen/docs/docs-reports.md) | Client reporting API |
| [`docs-user-auth.md`](../gen/docs/docs-user-auth.md) | Authentication API |

### Package Reference

| Package | Path | Description |
|---------|------|-------------|
| `prodapi-core` | [`prodapi-core/`](../prodapi-core/) | Core utilities (tracer, background, stepper) |
| `prodapi` | [`prodapi/`](../prodapi/) | Main library with full features |
| `prodapi-web` | [`prodapi-web/`](../prodapi-web/) | Web-specific conveniences |
| `prodapi-pg` | [`prodapi-pg/`](../prodapi-pg/) | PostgreSQL support |
| `prodapi-userauth` | [`prodapi-userauth/`](../prodapi-userauth/) | Authentication |
| `prodapi-proxy` | [`prodapi-proxy/`](../prodapi-proxy/) | Reverse proxy |
| `prodapi-gen` | [`prodapi-gen/`](../prodapi-gen/) | Documentation generators |
| `example` | [`example/`](../example/) | Example application |

## Documentation Structure

```
docs/
├── README.md              # This file - documentation index
├── AI-CONTEXT.md          # AI assistant bootstrap context
├── architecture.md        # System architecture
├── component-guide.md     # Component development guide
├── quickstart.md          # Quickstart tutorial
├── module-index.md        # Complete module reference
├── prodapi-components.png # Component diagram
├── prodapi-composition.png# Composition diagram
└── example-screenshot.png # Status page screenshot
```

## Key Concepts

### 1. Components

Services are built from **components** - self-contained units with handlers, background values, traces, counters, status pages, and health checks.

### 2. Contravariant Tracing

Logging is done via `Tracer m a` - a contravariant functor that allows flexible composition without committing to a backend.

### 3. Background Values

Shared state that updates asynchronously via `BackgroundVal a`.

### 4. Health Checks

Kubernetes-compatible liveness and readiness probes with dynamic condition management.

### 5. Status Pages

HTML/JSON status pages that compose monoidally.

## Quick Links

### Core Types

```haskell
-- Tracing
Tracer m a                    -- Contravariant logger

-- Background
BackgroundVal a               -- Async-updated value

-- Health
Runtime                       -- Health check runtime
Liveness                      -- Alive
Readiness                     -- Ready | Ill (Set Reason)

-- Status
Status a                      -- Status page data
RenderStatus a                -- Status -> Html ()

-- Stepper
StepIO a b                    -- Delayable state transitions
```

### Common Tasks

- **Create a new service**: See [`quickstart.md`](quickstart.md)
- **Build a component**: See [`component-guide.md`](component-guide.md)
- **Add health checks**: See [`gen/docs/docs-health.md`](../gen/docs/docs-health.md)
- **Add metrics**: See [`gen/docs/docs-prometheus.md`](../gen/docs/docs-prometheus.md)
- **Add authentication**: See [`gen/docs/docs-user-auth.md`](../gen/docs/docs-user-auth.md)

## Contributing to Documentation

When adding new features:

1. Update relevant docs in `docs/`
2. Update `AI-CONTEXT.md` with new types/patterns
3. Update `module-index.md` with new modules
4. Regenerate API docs if needed:
   ```bash
   cabal run prodapi-gen
   ```

## Getting Help

1. Check the relevant documentation file
2. Look at the example application
3. Check the generated API docs
4. File an issue with reproduction steps

---

*This documentation was bootstrapped during initial project setup.*
*Last updated: Project initialization*

