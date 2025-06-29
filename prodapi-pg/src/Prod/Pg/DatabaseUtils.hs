{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prod.Pg.DatabaseUtils where

import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import Data.Int (Int64)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (..), ToRow)
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import qualified Database.PostgreSQL.Simple.Types as Sql
import Prod.Tracer (Tracer (..), contramap)
import qualified System.Clock as Clock

-------------------------------------------------------------------------------

data PGConnectionTrace
    = RunQuery ByteString.ByteString
    | DoneQuery Clock.TimeSpec Clock.TimeSpec Int ByteString.ByteString
    | RunExec Sql.Query
    | DoneExec Clock.TimeSpec Clock.TimeSpec Int64 Sql.Query

instance Show PGConnectionTrace where
    show (RunQuery bs) = "`" <> C8.unpack bs <> "`"
    show (DoneQuery t0 t1 k _) = mconcat ["n-rows=", show k, "; delay=", show (Clock.diffTimeSpec t1 t0)]
    show (RunExec q) = "`" <> show q <> "`"
    show (DoneExec t0 t1 k _) = mconcat ["result=", show k, "; delay=", show (Clock.diffTimeSpec t1 t0)]

pgExec ::
    (ToRow q) =>
    Tracer IO PGConnectionTrace ->
    Connection ->
    Sql.Query ->
    q ->
    IO Int64
pgExec tracer conn q args = do
    runTracer tracer (RunExec q)
    t0 <- Clock.getTime Clock.Monotonic
    ret <- Sql.execute conn q args
    t1 <- Clock.getTime Clock.Monotonic
    runTracer tracer (DoneExec t0 t1 ret q)
    pure ret

pgQuery ::
    (ToRow q, FromRow r) =>
    Tracer IO PGConnectionTrace ->
    Connection ->
    Sql.Query ->
    q ->
    IO [r]
pgQuery tracer conn q args = do
    formatted <- Sql.formatQuery conn q args
    runTracer tracer (RunQuery formatted)
    t0 <- Clock.getTime Clock.Monotonic
    ret <- Sql.query conn q args
    t1 <- Clock.getTime Clock.Monotonic
    runTracer tracer (DoneQuery t0 t1 (length ret) formatted)
    pure ret

-------------------------------------------------------------------------------
data Trace
    = RefreshMview MviewName ConcurrentlyOrNot PGConnectionTrace
    | RefillMtable MtableFillingFunction PGConnectionTrace
    deriving (Show)

-------------------------------------------------------------------------------
newtype MviewName = MviewName ByteString.ByteString
    deriving (Show)

instance ToField MviewName where
    toField (MviewName n) = Plain (Builder.fromByteString n)

data ConcurrentlyOrNot
    = Concurrently
    | Blocking
    deriving (Show, Eq)

refreshMview ::
    Tracer IO Trace ->
    IO Connection ->
    MviewName ->
    ConcurrentlyOrNot ->
    IO ()
refreshMview tracer mkConn name c = do
    conn <- mkConn
    _ <- pgExec (contramap (RefreshMview name c) tracer) conn q (Only name) :: IO (Int64)
    pure ()
  where
    q = case c of
        Blocking -> [sql|REFRESH MATERIALIZED VIEW ? |]
        Concurrently -> [sql|REFRESH MATERIALIZED VIEW CONCURRENTLY ? |]

-------------------------------------------------------------------------------
newtype MtableFillingFunction = MtableFillingFunction ByteString.ByteString
    deriving (Show)

execFillMTable ::
    Tracer IO Trace ->
    IO Connection ->
    MtableFillingFunction ->
    IO ()
execFillMTable tracer mkConn selectfunc@(MtableFillingFunction sqlfun) = do
    conn <- mkConn
    _ <- pgQuery (contramap (RefillMtable selectfunc) tracer) conn q () :: IO [(Only ())]
    pure ()
  where
    q :: Sql.Query
    q = Sql.Query ("select " <> sqlfun <> "()")
