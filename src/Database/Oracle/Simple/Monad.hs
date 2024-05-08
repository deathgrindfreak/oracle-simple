{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Oracle.Simple.Monad
  ( OracleEnv,
    newOracleEnv,
    withOracleConnection,
    withLockedOracleConnection,
    withPoolConnection,
    MonadOracle,
    HasOracleContext (..),
    TransactionState (..),
    TransactionStateError (..),
    withNewTransactionState,
    getExecutionMode,
    task,
    ping,
    isHealthy,
  )
where

import Control.Concurrent.MVar (MVar)
import Control.Exception.Safe (Exception, MonadCatch, MonadThrow)
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Typeable as Typeable
import Database.Oracle.Simple.Internal (Connection)
import qualified Database.Oracle.Simple.Internal as Internal
import Database.Oracle.Simple.Pool (Pool, acquireConnection)
import Numeric.Natural
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO

data ConnectionState
  = NotConnected
  | Connected ConnectionContext

newtype SavePoint = SavePoint Natural
  deriving (Show, Eq)

newSavePoint :: SavePoint
newSavePoint = SavePoint 1

nextSavePoint :: SavePoint -> SavePoint
nextSavePoint (SavePoint n) = SavePoint (n + 1)

data TransactionState = OutermostTransaction | SavepointTransaction SavePoint
  deriving (Show, Eq)

newtype TransactionStateError = TransactionStateError String
  deriving (Show, Typeable.Typeable)

instance Exception TransactionStateError

data ConnectionContext = ConnectionContext
  { ccConnection :: Connection
  , ccConnectionUtilizationLock :: MVar ()
  , ccConnectionCloseLock :: MVar ()
  }

data OracleEnv = OracleEnv
  { dbEnvPool :: Pool
  , dbConnectionState :: ConnectionState
  , dbTransactionState :: Maybe TransactionState
  }

withNewTransactionState :: MonadOracle m => (TransactionState -> m a) -> m a
withNewTransactionState action = do
  mbTransactionState <- dbTransactionState <$> getOracleEnv
  let
    nextTransactionState =
      case mbTransactionState of
        Nothing -> OutermostTransaction
        Just OutermostTransaction -> SavepointTransaction newSavePoint
        Just (SavepointTransaction savePoint) ->
          SavepointTransaction (nextSavePoint savePoint)
  localOracleEnv
    (\env -> env {dbTransactionState = Just nextTransactionState})
    (action nextTransactionState)

newOracleEnv :: Pool -> OracleEnv
newOracleEnv pool = OracleEnv pool NotConnected Nothing

close :: MonadOracle m => ConnectionContext -> m ()
close conn =
  UnliftIO.mask $ \restore -> do
    mbCloseLock <- UnliftIO.tryTakeMVar (ccConnectionCloseLock conn)
    case mbCloseLock of
      Just () -> restore . MIO.liftIO $ Internal.close (ccConnection conn)
      Nothing -> pure ()

-- | Bracket a computation between acquiring a connection from a session pool and releasing the connection.
withPoolConnection ::
  MonadOracle m => (ConnectionContext -> m c) -> m c
withPoolConnection action = do
  pool <- dbEnvPool <$> getOracleEnv
  UnliftIO.bracket
    ( ConnectionContext
        <$> MIO.liftIO (acquireConnection pool)
        <*> UnliftIO.newMVar ()
        <*> UnliftIO.newMVar ()
    )
    close
    action

withSharedConnectionContext :: MonadOracle m => (ConnectionContext -> m a) -> m a
withSharedConnectionContext action = do
  dbEnv <- getOracleEnv
  case dbConnectionState dbEnv of
    Connected connCtx -> action connCtx
    NotConnected ->
      withPoolConnection $ \connCtx -> do
        localOracleEnv
          (\env -> env {dbConnectionState = Connected connCtx})
          (action connCtx)

withOracleConnection :: MonadOracle m => (Connection -> m a) -> m a
withOracleConnection action = withSharedConnectionContext (action . ccConnection)

withLockedOracleConnection :: MonadOracle m => (Connection -> m a) -> m a
withLockedOracleConnection action =
  withSharedConnectionContext $ \connCtx ->
    UnliftIO.withMVar (ccConnectionUtilizationLock connCtx) $ \() ->
      action (ccConnection connCtx)

-- While we'll probably always want to run a block of queries inside of a `task` or `withTransaction` block
-- We can change the execution mode to commit on a success when we're writing statements outside of said blocks
getExecutionMode :: MonadOracle m => m Internal.DPIModeExec
getExecutionMode = do
  env <- getOracleEnv
  pure $
    case dbTransactionState env of
      Just _ -> Internal.DPI_MODE_EXEC_DEFAULT
      Nothing -> Internal.DPI_MODE_EXEC_COMMIT_ON_SUCCESS

class
  ( Monad m
  , MIO.MonadIO m
  , HasOracleContext m
  , MonadThrow m
  , MonadCatch m
  , MonadFail m
  , MonadUnliftIO m
  ) =>
  MonadOracle m

class HasOracleContext m where
  getOracleEnv :: m OracleEnv
  localOracleEnv :: (OracleEnv -> OracleEnv) -> m a -> m a

instance (Monad m, HasOracleContext m) => HasOracleContext (ReaderT a m) where
  getOracleEnv = lift getOracleEnv
  localOracleEnv modEnv = mapReaderT (localOracleEnv modEnv)

-- Lift some functions for convenience
ping :: MonadOracle m => m Bool
ping = withOracleConnection (MIO.liftIO . Internal.ping)

isHealthy :: MonadOracle m => m Bool
isHealthy = withOracleConnection (MIO.liftIO . Internal.isHealthy)

-- A task merely threads the same connection to a block of actions
task :: MonadOracle m => m a -> m a
task action = do
  env <- getOracleEnv
  case dbTransactionState env of
    Just _ -> action
    Nothing -> withOracleConnection (const action)
