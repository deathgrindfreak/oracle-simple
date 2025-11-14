{-# LANGUAGE FlexibleContexts #-}

module Database.Oracle.Simple.Subscribe
  ( subscribe,
    subscribeIPv4,
    registerSubscription,
    withIPv4Subscription,
    withHTTPSubscription,
    withSubscription,
    register,
  )
where

import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Word (Word16)
import qualified Net.IPv4 as IPv4
import qualified UnliftIO

import Database.Oracle.Simple.Internal
  ( Connection,
    SqlStatement,
    SubscriberMessage,
    Subscription,
    SubscriptionCreateOptions,
    closeStatement,
    dpiExecute,
    subscribeCallbackDPI,
    subscribeHTTPDPI,
    subscribeIPv4DPI,
    subscriptionPrepareStatementDPI,
  )
import Database.Oracle.Simple.Monad
  ( MonadOracle,
    getExecutionMode,
    withOracleConnection,
  )

subscribe ::
  MonadOracle m =>
  Connection ->
  Maybe SubscriptionCreateOptions ->
  (SubscriberMessage -> m ()) ->
  m Subscription
subscribe conn options subscriptionCallback = do
  UnliftIO.withRunInIO $ \runInIO ->
    UnliftIO.liftIO $
      subscribeCallbackDPI
        conn
        options
        (runInIO . subscriptionCallback)

subscribeHTTP ::
  MonadOracle m =>
  Connection ->
  Maybe SubscriptionCreateOptions ->
  String ->
  m Subscription
subscribeHTTP conn options url =
  MIO.liftIO $ subscribeHTTPDPI conn options url

subscribeIPv4 ::
  MonadOracle m =>
  Connection ->
  Maybe SubscriptionCreateOptions ->
  IPv4.IPv4 ->
  Word16 ->
  m Subscription
subscribeIPv4 conn options ip port =
  MIO.liftIO $ subscribeIPv4DPI conn options ip port

registerSubscription ::
  MonadOracle m =>
  Subscription ->
  SqlStatement ->
  m ()
registerSubscription subscription sql = do
  mode <- getExecutionMode
  MIO.liftIO $ do
    stmt <- subscriptionPrepareStatementDPI subscription sql
    _ <- dpiExecute stmt mode
    closeStatement stmt

withSubscription ::
  (MonadOracle m) =>
  Maybe SubscriptionCreateOptions ->
  (SubscriberMessage -> m ()) ->
  ReaderT Subscription m a ->
  m a
withSubscription options cb action = do
  withOracleConnection $ \conn -> do
    subscription <- subscribe conn options cb
    runReaderT action subscription

withHTTPSubscription ::
  (MonadOracle m) =>
  Maybe SubscriptionCreateOptions ->
  String ->
  ReaderT Subscription m a ->
  m a
withHTTPSubscription options url action = do
  withOracleConnection $ \conn -> do
    subscription <- subscribeHTTP conn options url
    runReaderT action subscription

withIPv4Subscription ::
  (MonadOracle m) =>
  Maybe SubscriptionCreateOptions ->
  IPv4.IPv4 ->
  Word16 ->
  ReaderT Subscription m a ->
  m a
withIPv4Subscription options ip port action = do
  withOracleConnection $ \conn -> do
    subscription <- subscribeIPv4 conn options ip port
    runReaderT action subscription

register :: MonadOracle m => SqlStatement -> ReaderT Subscription m ()
register sql = do
  subscription <- ask
  lift $ registerSubscription subscription sql
