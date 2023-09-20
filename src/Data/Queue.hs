-- | Postgresql queue
module Data.Queue (Queue (..), run) where

import Codec.Serialise (Serialise, deserialise, serialise)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Database.Persist (Entity (entityKey, entityVal), PersistEntity (Key), PersistStoreWrite (delete, update), PersistValue (PersistUTCTime), (+=.), (=.))
import Database.Persist qualified as P
import Database.Persist.Sql (ConnectionPool, SqlPersistT, rawSql, runSqlPool)
import GHC.Conc (getNumProcessors)
import Model (EntityField (..), Jobs (jobsAttempts, jobsPayload))
import System.Random (randomRIO)
import Utils (repeatM, whileM)

data Queue m a e where
  Queue ::
    (MonadUnliftIO m, Serialise a, Serialise e) =>
    { queueDbPool :: ConnectionPool
    , queueHandler :: a -> m (Either e ())
    } ->
    Queue m a e

claim :: (MonadUnliftIO m, Serialise a) => SqlPersistT m (Maybe (Key Jobs, a))
claim = do
  time <- liftIO getCurrentTime
  jobs :: [Entity Jobs] <- rawSql "UPDATE jobs SET updated_at = ?, locked = 'T', locked_at = ? WHERE id IN (SELECT id FROM jobs WHERE locked = 'F' AND run_at <= ? ORDER BY run_at ASC, created_at ASC LIMIT 1) RETURNING ??" [PersistUTCTime time, PersistUTCTime time, PersistUTCTime time]
  case jobs of
    [job] -> return $ Just (entityKey job, deserialise $ fromStrict $ jobsPayload $ entityVal job)
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

runOne :: (MonadUnliftIO m, Serialise a, Serialise e) => Queue m a e -> m Bool
runOne queue = do
  let dbpool = queueDbPool queue
  claimed <- runSqlPool claim dbpool
  case claimed of
    Nothing -> return False
    Just (queueId, job) -> do
      result <- queueHandler queue job
      case result of
        Right _ -> runSqlPool (delete queueId) dbpool
        Left e -> runSqlPool (unclaim (queueId, e)) dbpool
      return True

runThread :: (MonadUnliftIO m, Serialise a, Serialise e) => Queue m e a -> m ()
runThread queue = do
  liftIO $ print ("Run queue…" :: Text)
  -- Run until queue is empty
  whileM $ runOne queue
  secs <- liftIO $ randomRIO (10 :: Int, 30)
  liftIO $ threadDelay (secs * 1_000_000)
  pass

run :: (MonadUnliftIO m, Serialise a, Serialise e) => Queue m e a -> (m Void -> IO Void) -> m ()
run queue wrapper = do
  concurrency <- liftIO getNumProcessors
  repeatM
    concurrency
    ( do
        _ <- liftIO $ forkIO $ do
          _ <- wrapper $ infinitely $ runThread queue
          pass
        pass
    )
  pass
