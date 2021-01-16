prod-api
========

A simple curation libraries for building Haskell services.

# Application

Typical applications would benefit from separating code in a few Haskell
modules:

```
  MyService/Base.hs
     # Defines the base types useful for your application.
     # Here you would import other libraries as well.

  MyService/Counters.hs
     # Defines a datatype with all Prometheus counters.
     # Also provides some helper functions to set/update some counters.

  MyService/Runtime.hs
     # Defines input parameters as well as runtime values such as connection to
     # databases, or background-updated values.

  MyService/Api.hs
     # Defines the service API using Servant.

  MyService.hs
     # Combines all the above and implements the proper handlers.
```

# Provided APIs and libraries

## defining APIs with Servant
- encourages the use of Servant to declare and implement handlers (offered components use Servant in a way or another)

## echo
- enables to test round-trips

## health-checking
- normalizes whether an application is up or not

## status
- identification, healthiness
- helpful /status page
- status-page is customizable forcing Lucid

## metrics
- exposes counters over Prometheus

## logging
- promotes the use of contravariant logging to avoid forcing an early choice on library users

## client-reporting
- simple API for clients to dial-in some timestamped logs (e.g., session digests, errors)

## primitive authentication
- PostgreSQL minimal identity management
- JWT claims in a Cookies
- Servant Combinators for Cookie-Protection

## background values and watchdogs
- ways to spawn asynchronously-updated values
- special case of watchdogs which touch a prometheus counter on success

# TODO

- split PostgreSQL and metrics to own packages
- curation choices around argument-parsing
- curation choices for code-generators
- curation choices for leader-elections
- scaffolder
