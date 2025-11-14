{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Avoid restricted function" -}

module Database.Oracle.Simple.Internal.Context
  ( defaultCommonCreateParams,
    defaultPoolCreateParams,
    withDefaultPoolCreateParams,
    withDefaultCommonCreateParams,
    withConnCreateParams,
    createContext,
    getClientVersion,
    getServerVersion,
    globalContext,
    throwOracleError,
    withCommonCreateParams,
    subscribeCallbackDPI,
    subscribeIPv4DPI,
    subscribeHTTPDPI,
  )
where

import Control.Exception.Safe (throwIO)
import Control.Monad (unless, void, (<=<))
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Set as Set
import Data.Word (Word16)
import Foreign.C.String (CString, peekCString, peekCStringLen, withCString, withCStringLen)
import Foreign.C.Types (CInt (..))
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable.Generic (Storable (..))
import qualified Net.IPv4 as IPv4
import System.IO.Unsafe (unsafePerformIO)

import Database.Oracle.Simple.Internal.Entity
  ( CommonCreateParams (..),
    Connection,
    CreateMode (..),
    DPICommonCreateParams (..),
    DPIConn,
    DPIConnectionCreateParams (..),
    DPIContext,
    DPIPoolCreateParams (..),
    DPISubscription,
    DPISubscriptionCreateParams (..),
    DPISubscriptionProtocol (..),
    ErrorInfo (..),
    OracleError (..),
    PoolCreateParams (..),
    SubscriberMessage,
    Subscription,
    SubscriptionCreateOptions (..),
    VersionInfo (..),
    createModeToDPIFlags,
    mkSubscriptionCallback,
    subscriptionOperationNotificationsToFlags,
    toDPIQOS,
    toFlags,
    toSubscriberMessage,
  )

globalContext :: IORef (Ptr DPIContext)
{-# NOINLINE globalContext #-}
globalContext = unsafePerformIO (newIORef =<< createContext)

defaultCommonCreateParams :: IO CommonCreateParams
defaultCommonCreateParams = do
  withDefaultCommonCreateParams $ \defaultCommonCreateParamsPtr -> do
    DPICommonCreateParams {..} <- peek defaultCommonCreateParamsPtr
    encoding <- peekCString dpi_encoding
    nencoding <- peekCString dpi_nencoding
    edition <- peekCStringLen (dpi_edition, fromIntegral dpi_editionLength)
    driverName <- peekCStringLen (dpi_driverName, fromIntegral dpi_driverNameLength)
    pure $
      CommonCreateParams
        { createMode = Set.singleton DefaultCreateMode
        , encoding
        , nencoding
        , edition
        , driverName
        , sodaMetadataCache = dpi_sodaMetadataCache
        , stmtCacheSize = fromIntegral dpi_stmtCacheSize
        }

defaultPoolCreateParams :: IO PoolCreateParams
defaultPoolCreateParams =
  withDefaultPoolCreateParams $ \defaultPoolCreateParamsPtr -> do
    DPIPoolCreateParams {..} <- peek defaultPoolCreateParamsPtr
    pure
      PoolCreateParams
        { minSessions = fromIntegral dpi_minSessions
        , maxSessions = fromIntegral dpi_maxSessions
        , sessionIncrement = fromIntegral dpi_sessionIncrement
        , pingInterval = fromIntegral dpi_pingInterval
        , pingTimeout = fromIntegral dpi_pingTimeout
        , homogeneous = fromIntegral dpi_homogeneous
        , getMode = dpi_getMode
        , timeout = fromIntegral dpi_timeout
        , waitTimeout = fromIntegral dpi_waitTimeout
        , maxLifetimeSession = fromIntegral dpi_maxLifetimeSession
        , maxSessionsPerShard = fromIntegral dpi_maxSessionsPerShard
        }

foreign import ccall "dpiContext_initPoolCreateParams"
  dpiContext_initPoolCreateParams ::
    Ptr DPIContext ->
    Ptr DPIPoolCreateParams ->
    IO Int

withDefaultPoolCreateParams :: (Ptr DPIPoolCreateParams -> IO a) -> IO a
withDefaultPoolCreateParams f = do
  ctx <- readIORef globalContext
  alloca $ \poolCreateParamsPtr -> do
    status <- dpiContext_initPoolCreateParams ctx poolCreateParamsPtr
    unless (status == 0) $ do
      throwIO . userError $ "non-zero pool create params status: " <> show status
    f poolCreateParamsPtr

foreign import ccall "dpiContext_initCommonCreateParams"
  dpiContext_initCommonCreateParams ::
    Ptr DPIContext ->
    Ptr DPICommonCreateParams ->
    IO Int

withDefaultCommonCreateParams :: (Ptr DPICommonCreateParams -> IO a) -> IO a
withDefaultCommonCreateParams f = do
  ctx <- readIORef globalContext
  alloca $ \commonCreateParamsPtr -> do
    status <- dpiContext_initCommonCreateParams ctx commonCreateParamsPtr
    unless (status == 0) $ do
      throwIO . userError $ "non-zero default common create params status: " <> show status
    f commonCreateParamsPtr

withCommonCreateParams ::
  Maybe CommonCreateParams ->
  (Ptr DPICommonCreateParams -> IO a) ->
  IO a
withCommonCreateParams mbCommonCreateParams f =
  case mbCommonCreateParams of
    Nothing -> f nullPtr
    Just CommonCreateParams {..} ->
      withDefaultCommonCreateParams $ \defaultCommonCreateParamsPtr -> do
        commonCreateParams <- peek defaultCommonCreateParamsPtr

        withCString encoding $ \encodingCString ->
          withCString nencoding $ \nencodingCString ->
            withCStringLen edition $ \(editionCString, fromIntegral -> editionLen) ->
              withCStringLen driverName $ \(driverNameCString, fromIntegral -> driverNameLen) ->
                poke
                  defaultCommonCreateParamsPtr
                  commonCreateParams
                    { dpi_createMode = createModeToDPIFlags createMode
                    , dpi_encoding = encodingCString
                    , dpi_nencoding = nencodingCString
                    , dpi_edition = editionCString
                    , dpi_editionLength = editionLen
                    , dpi_driverName = driverNameCString
                    , dpi_driverNameLength = driverNameLen
                    , dpi_sodaMetadataCache = sodaMetadataCache
                    , dpi_stmtCacheSize = fromIntegral stmtCacheSize
                    }
        f defaultCommonCreateParamsPtr

foreign import ccall "context_create"
  dpiContext_create ::
    -- | major version
    CInt ->
    -- | minor version
    CInt ->
    -- | context return
    Ptr (Ptr DPIContext) ->
    -- | error info struct
    Ptr ErrorInfo ->
    IO Int

createContext :: IO (Ptr DPIContext)
createContext = do
  alloca $ \contextPtrPtr -> do
    alloca $ \errorInfoPtr -> do
      majorVersion <- getMajorVersion
      minorVersion <- getMinorVersion
      statusCode <-
        dpiContext_create
          majorVersion
          minorVersion
          contextPtrPtr
          errorInfoPtr
      if statusCode == 0
        then peek contextPtrPtr
        else (throwIO <=< toOracleError <=< peek) errorInfoPtr

foreign import ccall "dpiContext_getClientVersion"
  dpiContext_getClientVersion ::
    Ptr DPIContext ->
    Ptr VersionInfo ->
    IO Int

getClientVersion :: IO VersionInfo
getClientVersion = do
  ctx <- readIORef globalContext
  alloca $ \versionPtr -> do
    statusCode <- dpiContext_getClientVersion ctx versionPtr
    if statusCode == 0
      then peek versionPtr
      else throwIO . userError $ "getClientVersion: " <> show statusCode

foreign import ccall "dpiConn_getServerVersion"
  dpiContext_getServerVersion ::
    Ptr DPIConn ->
    Ptr CString ->
    CInt ->
    Ptr VersionInfo ->
    IO Int

getServerVersion :: Connection -> VersionInfo -> IO String
getServerVersion fptr versionInfo = do
  withForeignPtr fptr $ \conn ->
    alloca $ \releaseStringPtr -> do
      alloca $ \versionInfoPtr -> do
        poke versionInfoPtr versionInfo
        status <-
          dpiContext_getServerVersion
            conn
            releaseStringPtr
            (fromIntegral (10 :: Int))
            versionInfoPtr
        if status == 0
          then (peekCString <=< peek) releaseStringPtr
          else throwIO . userError $ "getServerVersion: " <> show status

foreign import ccall "dpiContext_initConnCreateParams"
  dpiContext_initConnCreateParams ::
    Ptr DPIContext ->
    Ptr DPIConnectionCreateParams ->
    IO Int

withConnCreateParams :: (DPIConnectionCreateParams -> IO a) -> IO a
withConnCreateParams f = do
  ctx <- readIORef globalContext
  alloca $ \connCreateParamsPtr -> do
    status <- dpiContext_initConnCreateParams ctx connCreateParamsPtr
    unless (status == 0) $ do
      throwIO . userError $ "conn create params isn't 0" <> show status
    f =<< peek connCreateParamsPtr

foreign import ccall "dpiContext_getError"
  dpiContext_getError :: Ptr DPIContext -> Ptr ErrorInfo -> IO ()

getErrorInfo :: IO ErrorInfo
getErrorInfo = do
  ctx <- readIORef globalContext
  alloca $ \errorInfoPtr -> do
    dpiContext_getError ctx errorInfoPtr
    peek errorInfoPtr

throwOracleError :: Integral i => i -> IO ()
throwOracleError returnCode = do
  unless (returnCode == 0) $
    (throwIO =<< toOracleError =<< getErrorInfo)

foreign import ccall "getMajorVersion" getMajorVersion :: IO CInt
foreign import ccall "getMinorVersion" getMinorVersion :: IO CInt

toOracleError :: ErrorInfo -> IO OracleError
toOracleError ErrorInfo {..} = do
  oracleErrorFnName <- peekCString errorInfoFnName
  oracleErrorAction <- peekCString errorInfoAction
  oracleErrorMessage <- peekCStringLen (errorInfoMessage, fromIntegral errorInfoMessageLength)
  oracleErrorSqlState <- peekCString errorInfoSqlState

  let
    intToBool :: Int -> Bool
    intToBool 0 = False
    intToBool 1 = True
    intToBool i = error $ "boolean encoded as integer not 0 or 1: " <> show i

    oracleErrorCode = fromIntegral errorInfoCode
    oracleErrorIsRecoverable = intToBool $ fromIntegral errorInfoIsRecoverable
    oracleErrorIsWarning = intToBool $ fromIntegral errorInfoIsWarning

  pure OracleError {..}

foreign import ccall "dpiContext_initSubscrCreateParams"
  dpiContext_initSubscrCreateParams ::
    Ptr DPIContext ->
    Ptr DPISubscriptionCreateParams ->
    IO Int

withDefaultSubscriptionCreateParams ::
  (Ptr DPISubscriptionCreateParams -> IO a) ->
  IO a
withDefaultSubscriptionCreateParams cb = do
  ctx <- readIORef globalContext
  alloca $ \subscriptionCreateParamsPtr -> do
    status <- dpiContext_initSubscrCreateParams ctx subscriptionCreateParamsPtr
    unless (status == 0) $ do
      throwIO . userError $ "non-zero subscription create params status: " <> show status
    cb subscriptionCreateParamsPtr

foreign import ccall "dpiConn_subscribe"
  dpiConn_subscribe ::
    Ptr DPIConn ->
    Ptr DPISubscriptionCreateParams ->
    Ptr (Ptr DPISubscription) ->
    IO Int

foreign import ccall "dpiConn_unsubscribe"
  dpiConn_unsubscribe ::
    Ptr DPIConn ->
    Ptr DPISubscription ->
    IO Int

foreign import ccall "dpiSubscr_release"
  dpiSubscr_release ::
    Ptr DPISubscription ->
    IO Int

setOptions ::
  Maybe SubscriptionCreateOptions ->
  DPISubscriptionCreateParams ->
  DPISubscriptionCreateParams
setOptions mbOpts createParams =
  createParams
    { dpi_operations =
        maybe
          (dpi_operations createParams)
          (subscriptionOperationNotificationsToFlags . operations)
          mbOpts
    , dpi_qos = maybe (dpi_qos createParams) (toFlags toDPIQOS . qos) mbOpts
    }

subscribeCallbackDPI ::
  Connection ->
  Maybe SubscriptionCreateOptions ->
  (SubscriberMessage -> IO ()) ->
  IO Subscription
subscribeCallbackDPI connPtr options subscriptionCallback =
  withForeignPtr connPtr $ \conn -> do
    withDefaultSubscriptionCreateParams $ \defaultSubParamsPtr -> do
      alloca $ \subPtrPtr -> do
        cb <-
          mkSubscriptionCallback $ \_ dpiMsgPtr -> do
            dpiMsg <- peek dpiMsgPtr
            msg <- toSubscriberMessage dpiMsg
            subscriptionCallback msg

        defaultSubParams <- peek defaultSubParamsPtr
        poke defaultSubParamsPtr $
          (setOptions options defaultSubParams)
            { dpi_callback = cb
            , dpi_clientInitiated = fromBool True
            }

        throwOracleError =<< dpiConn_subscribe conn defaultSubParamsPtr subPtrPtr

        subPtr <- peek subPtrPtr
        newForeignPtr subPtr $ do
          freeHaskellFunPtr cb
          void $ dpiConn_unsubscribe conn subPtr
          void $ dpiSubscr_release subPtr

subscribeHTTPDPI ::
  Connection ->
  Maybe SubscriptionCreateOptions ->
  String ->
  IO Subscription
subscribeHTTPDPI connPtr options url =
  withForeignPtr connPtr $ \conn -> do
    withDefaultSubscriptionCreateParams $ \defaultSubParamsPtr -> do
      withCStringLen url $ \(urlStr, urlStrLen) -> do
        alloca $ \subPtrPtr -> do
          defaultSubParams <- peek defaultSubParamsPtr
          poke defaultSubParamsPtr $
            (setOptions options defaultSubParams)
              { dpi_subscrProtocol = DPI_SUBSCR_PROTO_HTTP
              , dpi_recipientName = urlStr
              , dpi_recipientNameLength = fromIntegral urlStrLen
              , dpi_groupingClass = 1 -- If this is not set it seg faults
              , dpi_clientInitiated = fromBool True
              }

          throwOracleError =<< dpiConn_subscribe conn defaultSubParamsPtr subPtrPtr

          subPtr <- peek subPtrPtr
          newForeignPtr subPtr $ do
            void $ dpiSubscr_release subPtr

subscribeIPv4DPI ::
  Connection ->
  Maybe SubscriptionCreateOptions ->
  IPv4.IPv4 ->
  Word16 ->
  IO Subscription
subscribeIPv4DPI connPtr options ipAddress portNumber =
  withForeignPtr connPtr $ \conn -> do
    withDefaultSubscriptionCreateParams $ \defaultSubParamsPtr -> do
      withCStringLen (IPv4.encodeString ipAddress) $ \(ip, ipLen) -> do
        alloca $ \subPtrPtr -> do
          defaultSubParams <- peek defaultSubParamsPtr
          poke defaultSubParamsPtr $
            (setOptions options defaultSubParams)
              { dpi_ipAddress = ip
              , dpi_ipAddressLength = fromIntegral ipLen
              , dpi_portNumber = fromIntegral portNumber
              , dpi_clientInitiated = fromBool True
              }

          throwOracleError =<< dpiConn_subscribe conn defaultSubParamsPtr subPtrPtr

          subPtr <- peek subPtrPtr
          newForeignPtr subPtr $ do
            void $ dpiSubscr_release subPtr
