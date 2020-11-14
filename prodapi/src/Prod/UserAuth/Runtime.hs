{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Prod.UserAuth.Runtime
  ( Counters (..),
    initRuntime,
    Runtime,
    counters,
    secretstring,
    withConn,
    Connection,
    tokenValidityDuration,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, execute, query, rollback, withTransaction)
import Prod.UserAuth.Base (Minutes)
import Prod.UserAuth.Counters (Counters (..), initCounters)

data Runtime
  = Runtime
      { counters :: !Counters,
        secretstring :: !Text,
        connstring :: !ByteString
      }

initRuntime :: Text -> ByteString -> IO Runtime
initRuntime skret cstring =
  Runtime
    <$> initCounters
    <*> pure skret
    <*> pure cstring

withConn :: Runtime -> (Connection -> IO a) -> IO a
withConn runtime act = do
  conn <- connectPostgreSQL (connstring runtime)
  !ret <- act conn
  close conn
  pure ret

tokenValidityDuration :: Minutes
tokenValidityDuration = 30
