{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- todo: concurrency control with
-- \* monotonic version number and dead/alive checks
-- \* association to locks
module Prod.Pg.TaskQueue where

import qualified Data.Aeson as Aeson
import Data.Foldable (for_)
import Data.Int (Int64)
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (Connection, Only (..))
import Database.PostgreSQL.Simple.Newtypes as SqlNewtypes
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Prod.Stepper (Delayable (..), StepIO)
import qualified Prod.Stepper as Stepper
import Prod.Tracer (Tracer (..), contramap)

-------------------------------------------------------------------------------
import Prod.Pg.DatabaseUtils (PGConnectionTrace, pgQuery)

-------------------------------------------------------------------------------
type TaskId = Int64

data TaskHandle task
    = TaskHandle
    { taskId :: TaskId
    , taskDefinition :: task
    , markStarted :: Connection -> IO ()
    , markSuspended :: Connection -> IO ()
    , markFinished :: Connection -> IO ()
    }

data PGTrace
    = Enqueue PGConnectionTrace
    | Enqueued TaskId
    | NextTask PGConnectionTrace
    | GotNextTask (Maybe TaskId)
    | UpdateStatus Status TaskId PGConnectionTrace
    | UpdatedTask Status (Maybe TaskId)
    deriving (Show)

enqueue ::
    forall task.
    (Aeson.ToJSON task) =>
    Tracer IO PGTrace ->
    Connection ->
    task ->
    IO ()
enqueue tracer conn task = do
    let args = (Only $ SqlNewtypes.Aeson task)
    xs :: [(Only TaskId)] <- pgQuery (contramap Enqueue tracer) conn q args
    for_ xs $ \(Only x) ->
        runTracer tracer (Enqueued x)
  where
    q =
        [sql|INSERT INTO task_queue(status, priority, payload)
               VALUES ('new', 50, ?)
               RETURNING (id) |]

nextTask ::
    forall task.
    (Typeable task, Aeson.FromJSON task) =>
    Tracer IO PGTrace ->
    Connection ->
    IO (Maybe (TaskHandle task))
nextTask tracer conn = do
    let args = (Only ("6 hours" :: Text.Text))
    xs :: [(TaskId, SqlNewtypes.Aeson task)] <- pgQuery (contramap NextTask tracer) conn q args
    let xTask = Maybe.listToMaybe xs
    runTracer tracer (GotNextTask $ fmap fst xTask)
    return $ do
        (tId, payload) <- xTask
        pure $
            TaskHandle
                tId
                (SqlNewtypes.getAeson payload)
                (updateTaskStatus tracer "started" tId)
                (updateTaskStatus tracer "suspended" tId)
                (updateTaskStatus tracer "finished" tId)
  where
    q =
        [sql|WITH pick1 AS (
               SELECT id, payload
               FROM task_queue
               WHERE status IN ('new','suspended')
               ORDER BY priority ASC, created_at DESC
               LIMIT 1
             ), pick2 AS (
               SELECT id, payload
               FROM task_queue
               WHERE status IN ('started') AND age(updated_at) > ?
                 AND NOT (EXISTS (SELECT * FROM pick1))
               ORDER BY priority ASC, updated_at ASC
               LIMIT 1
             )
             SELECT * FROM pick1
               UNION
             SELECT * FROM pick2
             LIMIT 1|]

type Status = Text.Text
type Priority = Text.Text

updateTaskStatus ::
    Tracer IO PGTrace ->
    Status ->
    TaskId ->
    Connection ->
    IO ()
updateTaskStatus tracer st tId conn = do
    let args = (st, priorityIncrement, tId)
    xs :: [(Only Int64)] <- pgQuery (contramap (UpdateStatus st tId) tracer) conn q args
    let xTask = Maybe.listToMaybe xs
    runTracer tracer (UpdatedTask st (fmap fromOnly xTask))
  where
    priorityIncrement :: Int64
    priorityIncrement = case st of "started" -> 20; "suspended" -> 100; _ -> 0
    q =
        [sql|UPDATE task_queue
             SET updated_at = NOW(), status = ?, priority = priority + ?
             WHERE id = ?
             RETURNING id|]

-------------------------------------------------------------------------------

data Step task
    = LookupNextTask
    | ClaimTask (TaskHandle task)
    | WorkingOnTask (TaskHandle task)
instance Show (Step task) where
    show LookupNextTask = "LookupNextTask"
    show (ClaimTask t) = "ClaimTask { taskId = " <> show t.taskId <> " }"
    show (WorkingOnTask t) = "WorkingOnTask { " <> show t.taskId <> " }"

data Trace task
    = StepperTrace (Stepper.Trace (Step task) ())
    | TraceSqlStatement Text.Text
    | PrimitiveSql PGTrace
    deriving (Show)

data RunTaskResult
    = Unstarted
    | Final

data RunTask = RunTask

runTask ::
    forall task.
    (Typeable task, Aeson.FromJSON task, Aeson.ToJSON task) =>
    Tracer IO (Trace task) ->
    IO Connection ->
    (task -> Stepper.ExecFunctions RunTaskResult -> IO ()) ->
    Stepper.BaseStepIO RunTask (Stepper.Delayable RunTaskResult)
runTask tracer mkConnection performTask = \complete _ -> do
    lookupNextTask
        (complete $ Inline Unstarted)
        (claimTask (workOnTask complete))
        (Inline ())
  where
    execution :: (a -> Step task) -> (Stepper.ExecFunctions b -> a -> IO ()) -> StepIO a b
    execution f1 =
        Stepper.defineExecution (contramap StepperTrace tracer) f1 (const ())

    lookupNextTask :: IO () -> StepIO () (TaskHandle task)
    lookupNextTask complete = execution (const LookupNextTask) $ \handle () -> do
        t <- nextTask (contramap PrimitiveSql tracer) =<< mkConnection
        case t of
            Nothing -> print ("could not load task" :: String) >> complete
            Just h -> handle.inline h

    claimTask :: StepIO (TaskHandle task) (TaskHandle task)
    claimTask = execution ClaimTask $ \handle task -> do
        task.markStarted =<< mkConnection
        handle.inline task

    workOnTask :: StepIO (TaskHandle task) RunTaskResult
    workOnTask = execution WorkingOnTask $ \handle task -> do
        let f1 v = mkConnection >>= task.markFinished >> handle.inline v
        let f2 delaySpec v = mkConnection >>= task.markSuspended >> handle.delay delaySpec v
        performTask task.taskDefinition (Stepper.ExecFunctions f1 f2)
