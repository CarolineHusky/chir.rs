-- | Postgresql queue
module Data.Queue (Queue (..), run, scheduleTask, addTask, runTaskIn) where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Concurrent (threadDelay)
import Control.Monad.Logger (MonadLoggerIO, logDebug, logError)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.ByteString qualified as BS
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Database.Persist (Entity (entityKey, entityVal), PersistEntity (Key), PersistQueryWrite (updateWhere), PersistStoreWrite (delete, update), PersistValue (PersistText, PersistUTCTime), (+=.), (<=.), (=.))
import Database.Persist qualified as P
import Database.Persist.Sql (ConnectionPool, SqlPersistT, rawSql, runSqlPool)
import GHC.Conc (getNumProcessors)
import Model (EntityField (..), Jobs (..))
import System.Random (randomRIO)
import Utils (forkM, repeatM, timeoutM, whileM)

data Queue m a e where
  Queue ::
    (MonadUnliftIO m, Serialise a, Serialise e) =>
    { queueDbPool :: ConnectionPool
    , queueHandler :: a -> m (Either e ())
    , queueNodeName :: Text
    } ->
    Queue m a e

claim :: (MonadLoggerIO m, MonadUnliftIO m, Serialise a) => Queue m a e -> SqlPersistT m (Maybe (Key Jobs, a))
claim queue = do
  time <- liftIO getCurrentTime
  jobs :: [Entity Jobs] <- rawSql "UPDATE jobs SET updated_at = ?, locked = 'T', locked_at = ?, locked_by = ? WHERE id IN (SELECT id FROM jobs WHERE locked = 'F' AND run_at <= ? ORDER BY run_at ASC, created_at ASC LIMIT 1) RETURNING ??" [PersistUTCTime time, PersistUTCTime time, PersistText $ queueNodeName queue, PersistUTCTime time]
  case jobs of
    [job] -> case deserialiseOrFail $ toLazy $ jobsPayload $ entityVal job of
      Right value -> return $ Just (entityKey job, value)
      Left e -> do
        $(logError) $ "Failed to claim job: " <> show e
        return Nothing
    [] -> return Nothing
    _ -> error "Database error"

unclaim :: (MonadUnliftIO m, Serialise e) => (Key Jobs, e) -> SqlPersistT m ()
unclaim (queueId, value) = do
  time <- liftIO getCurrentTime
  current <-
    P.get queueId >>= \case
      Just current -> return current
      Nothing -> error "Missing queue item"
  -- exponential backoff for delay
  let nextDelaySecs = round (min 604800 $ 60 * 2 ** fromIntegral (jobsAttempts current) :: Double)
  let nextDelay :: NominalDiffTime = fromInteger nextDelaySecs
  let runAt = addUTCTime nextDelay time
  update
    queueId
    [ JobsUpdatedAt =. time
    , JobsRunAt =. runAt
    , JobsLastError =. toStrict (serialise value)
    , JobsAttempts +=. 1
    , JobsLocked =. False
    , JobsLocked_at =. Nothing
    , JobsLocked_by =. Nothing
    ]

runOne :: (MonadLoggerIO m, MonadUnliftIO m, Serialise a, Serialise e) => Queue m a e -> m Bool
runOne queue = do
  let dbpool = queueDbPool queue
  claimed <- runSqlPool (claim queue) dbpool
  case claimed of
    Nothing -> return False
    Just (queueId, job) -> do
      result <- timeoutM 300_000_000 $ queueHandler queue job
      case result of
        Just (Right _) -> runSqlPool (delete queueId) dbpool
        v -> runSqlPool (unclaim (queueId, v)) dbpool
      return True

runThread :: (MonadLoggerIO m, MonadUnliftIO m, Serialise a, Serialise e) => Queue m a e -> m ()
runThread queue = do
  secs <- liftIO $ randomRIO (10 :: Int, 30)
  liftIO $ threadDelay (secs * 1_000_000)
  $(logDebug) "Run queue…"
  -- Run until queue is empty
  whileM $ runOne queue
  pass

cleanupThread :: (MonadLoggerIO m, MonadUnliftIO m, Serialise a, Serialise e) => Queue m a e -> m ()
cleanupThread queue = do
  $(logDebug) "Cleanup queue"
  now <- liftIO getCurrentTime
  let oldTime = addUTCTime (-360 :: NominalDiffTime) now
  runSqlPool (updateWhere [JobsLocked_at <=. Just oldTime] [JobsUpdatedAt =. now, JobsLocked =. False, JobsLocked_at =. Nothing, JobsLocked_by =. Nothing]) (queueDbPool queue)
  liftIO $ threadDelay 360_000_000

run :: (MonadLoggerIO m, MonadUnliftIO m, Serialise a, Serialise e) => Queue m a e -> m ()
run queue = do
  concurrency <- liftIO getNumProcessors
  repeatM concurrency $ forkM $ infinitely $ runThread queue
  forkM $ infinitely $ cleanupThread queue
  pass

scheduleTask :: (MonadUnliftIO m, Serialise a) => a -> UTCTime -> SqlPersistT m ()
scheduleTask task at = do
  now <- liftIO getCurrentTime
  P.insert_ $ Jobs now now at (toStrict $ serialise task) BS.empty 0 False Nothing Nothing
  pass

addTask :: (MonadUnliftIO m, Serialise a) => a -> SqlPersistT m ()
addTask task = do
  now <- liftIO getCurrentTime
  scheduleTask task now

runTaskIn :: (MonadUnliftIO m, Serialise a) => a -> NominalDiffTime -> SqlPersistT m ()
runTaskIn task in_time = do
  now <- liftIO getCurrentTime
  scheduleTask task (addUTCTime in_time now)
