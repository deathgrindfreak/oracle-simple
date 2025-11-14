{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Oracle.Simple.Internal.Connection
  ( withConnection,
    connect,
    close,
    prepareStmt,
    ping,
    isHealthy,
    dpiConn_newVar,
    dpiConn_close_finalizer,
    dpiConn_release_finalizer,
  )
where

import Control.Exception.Safe (bracket)
import Data.IORef (readIORef)
import Foreign (peek)
import Foreign.C.String (CString, withCStringLen)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.ForeignPtr (addForeignPtrFinalizer, finalizeForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)

import Database.Oracle.Simple.Internal.Context (globalContext, throwOracleError, withCommonCreateParams)
import Database.Oracle.Simple.Internal.Entity
  ( Connection,
    ConnectionParams (..),
    DPICommonCreateParams (..),
    DPIConn,
    DPIConnectionCreateParams (..),
    DPIContext,
    DPIStmt,
    ODPICData,
    ODPICVar,
    SqlStatement,
  )

-- | Brackets a computation between opening and closing a connection.
withConnection :: ConnectionParams -> (Connection -> IO c) -> IO c
withConnection params = bracket (connect params) close

foreign import ccall "dpiConn_create"
  dpiConn_create ::
    -- | const dpiContext *context
    Ptr DPIContext ->
    -- | const char *userName
    CString ->
    -- | uint32_t userNameLength
    CUInt ->
    -- | const char *password
    CString ->
    -- | uint32_t passwordLength
    CUInt ->
    -- | const char *connectString
    CString ->
    -- | uint32_t conn length
    CUInt ->
    -- | const dpiCommonCreateParams *commonParams
    Ptr DPICommonCreateParams ->
    -- | const dpiConnCreateParams *createParams
    Ptr DPIConnectionCreateParams ->
    -- | dpi * conn
    Ptr (Ptr DPIConn) ->
    IO CInt

connectDPI :: ConnectionParams -> IO (Ptr DPIConn)
connectDPI ConnectionParams {..} = do
  ctx <- readIORef globalContext
  alloca $ \connPtr -> do
    withCStringLen user $ \(userCString, fromIntegral -> userLen) ->
      withCStringLen pass $ \(passCString, fromIntegral -> passLen) ->
        withCStringLen connString $ \(connCString, fromIntegral -> connLen) -> do
          withCommonCreateParams commonCreateParams $ \commonCreateParamsPtr -> do
            throwOracleError
              =<< dpiConn_create
                ctx
                userCString
                userLen
                passCString
                passLen
                connCString
                connLen
                commonCreateParamsPtr
                nullPtr
                connPtr
            peek connPtr

{- | The order that the finalizers are declared in is very important
The close must be defined /last/ so it can run /first/
Per the docs, "The finalizer will run before all other finalizers for the same object which have already been registered."
-}
connect :: ConnectionParams -> IO Connection
connect params = do
  connPtr <- connectDPI params
  fptr <- newForeignPtr_ connPtr
  addForeignPtrFinalizer dpiConn_release_finalizer fptr
  addForeignPtrFinalizer dpiConn_close_finalizer fptr
  pure fptr

foreign import ccall "&finalize_connection_default"
  dpiConn_close_finalizer :: FunPtr (Ptr DPIConn -> IO ())

foreign import ccall "&dpiConn_release"
  dpiConn_release_finalizer :: FunPtr (Ptr DPIConn -> IO ())

-- | An explicit call to 'close' will invoke the finalizers before the GC does
close :: Connection -> IO ()
close = finalizeForeignPtr

foreign import ccall "dpiConn_prepareStmt"
  dpiConn_prepareStmt ::
    Ptr DPIConn ->
    CInt ->
    CString ->
    CUInt ->
    CString ->
    CUInt ->
    Ptr (Ptr DPIStmt) ->
    IO CInt

prepareStmt ::
  Connection ->
  -- | sql
  SqlStatement ->
  IO (Ptr DPIStmt)
prepareStmt fptr sql = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \stmtPtr -> do
      withCStringLen sql $ \(sqlCStr, fromIntegral -> sqlCStrLen) -> do
        status <- dpiConn_prepareStmt conn 0 sqlCStr sqlCStrLen nullPtr 0 stmtPtr
        throwOracleError status
        peek stmtPtr

foreign import ccall "dpiConn_ping"
  dpiConn_ping ::
    Ptr DPIConn ->
    IO CInt

-- | Ping the connection to see if it is still alive
ping :: Connection -> IO Bool
ping fptr =
  withForeignPtr fptr $ fmap (== 0) . dpiConn_ping

-- | DPI_EXPORT int dpiConn_getIsHealthy(dpiConn *conn, int *isHealthy);
foreign import ccall "dpiConn_getIsHealthy"
  dpiConn_getIsHealthy ::
    Ptr DPIConn ->
    Ptr CInt ->
    IO CInt

-- | A pointer to an integer defining whether the connection is healthy (1) or not (0), which will be populated upon successful completion of this function.
isHealthy :: Connection -> IO Bool
isHealthy fptr =
  withForeignPtr fptr $ \conn -> do
    alloca $ \healthPtr -> do
      throwOracleError =<< dpiConn_getIsHealthy conn healthPtr
      (== 1) <$> peek healthPtr

-- | Variables that can be referenced in parameter binds
foreign import ccall "dpiConn_newVar"
  dpiConn_newVar ::
    -- | dpiConn *conn
    Ptr DPIConn ->
    -- | dpiOracleTypeNum oracleTypeNum
    CUInt ->
    -- | dpiNativeTypeNum nativeTypeNum
    CUInt ->
    -- | uint32_t maxArraySize
    CUInt ->
    -- | uint32_t size
    CUInt ->
    -- | int sizeIsBytes
    Bool ->
    -- | int isArray
    Bool ->
    -- | TODO dpiObjectType *objType
    Ptr CUInt ->
    -- | dpiVar **var (OUT)
    Ptr (Ptr ODPICVar) ->
    -- | dpiData **data (OUT)
    Ptr (Ptr ODPICData) ->
    -- | int
    IO CInt
