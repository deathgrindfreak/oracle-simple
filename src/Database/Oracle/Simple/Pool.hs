{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Oracle.Simple.Pool
  ( Pool,
    createPool,
    acquireConnection,
    withPool,
    closePool,
  )
where

import Control.Exception.Safe (bracket)
import Data.IORef (readIORef)
import Foreign
  ( ForeignPtr,
    FunPtr,
    Ptr,
    addForeignPtrFinalizer,
    alloca,
    finalizeForeignPtr,
    newForeignPtr_,
    nullPtr,
    peek,
    withForeignPtr,
  )
import Foreign.C (CInt (CInt), CString, CUInt (CUInt), withCString, withCStringLen)
import Foreign.Storable (poke)

import Database.Oracle.Simple.Internal
  ( CommonCreateParams (..),
    Connection (Connection),
    ConnectionParams (..),
    DPICommonCreateParams (..),
    DPIConn (DPIConn),
    DPIContext (DPIContext),
    DPIPool (DPIPool),
    DPIPoolCreateParams (..),
    PoolCreateParams (..),
    dpiConn_close_finalizer,
    dpiConn_release_finalizer,
    globalContext,
    throwOracleError,
    withDefaultCommonCreateParams,
    withDefaultPoolCreateParams,
  )

-- | A session pool; a group of stateless connections ("sessions") to the database.
newtype Pool = Pool (ForeignPtr DPIPool)
  deriving (Show, Eq)

-- | Creates and maintains a group of stateless connections to the database.
createPool ::
  ConnectionParams ->
  IO Pool
createPool params = do
  ctx <- readIORef globalContext
  DPIPool poolPtr <- alloca $ \connPtr -> do
    withCStringLen (user params) $ \(userCString, fromIntegral -> userLen) ->
      withCStringLen (pass params) $ \(passCString, fromIntegral -> passLen) ->
        withCStringLen (connString params) $ \(connCString, fromIntegral -> connLen) ->
          withCommonCreateParams (commonCreateParams params) $ \commonCreateParamsPtr ->
            withCreatePoolParams (createPoolParams params) $ \createConnParamsPtr -> do
              status <-
                dpiPool_create
                  ctx
                  userCString
                  userLen
                  passCString
                  passLen
                  connCString
                  connLen
                  commonCreateParamsPtr
                  createConnParamsPtr
                  connPtr

              throwOracleError status
              peek connPtr
  fptr <- newForeignPtr_ poolPtr
  addForeignPtrFinalizer dpiPool_release_finalizer fptr
  addForeignPtrFinalizer dpiPool_close_finalizer fptr
  pure (Pool fptr)

withCommonCreateParams ::
  Maybe CommonCreateParams ->
  (Ptr DPICommonCreateParams -> IO a) ->
  IO a
withCommonCreateParams mbCommonCreateParams f =
  case mbCommonCreateParams of
    Nothing -> f nullPtr
    Just CommonCreateParams {..} ->
      withDefaultCommonCreateParams $ \defaultCommonCreateParamsPtr -> do
        defaultCommonCreateParams <- peek defaultCommonCreateParamsPtr

        withCString encoding $ \encodingCString ->
          withCString nencoding $ \nencodingCString ->
            withCStringLen edition $ \(editionCString, fromIntegral -> editionLen) ->
              withCStringLen driverName $ \(driverNameCString, fromIntegral -> driverNameLen) ->
                poke
                  defaultCommonCreateParamsPtr
                  defaultCommonCreateParams
                    { dpi_createMode = createMode
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

withCreatePoolParams ::
  Maybe PoolCreateParams ->
  (Ptr DPIPoolCreateParams -> IO a) ->
  IO a
withCreatePoolParams mbCreateConnectionParams f =
  case mbCreateConnectionParams of
    Nothing -> f nullPtr
    Just addParams ->
      withDefaultPoolCreateParams $ \defaultPoolParmsPtr -> do
        defaultPoolParams <- peek defaultPoolParmsPtr

        poke
          defaultPoolParmsPtr
          defaultPoolParams
            { dpi_minSessions = fromIntegral $ minSessions addParams
            , dpi_maxSessions = fromIntegral $ maxSessions addParams
            , dpi_sessionIncrement = fromIntegral $ sessionIncrement addParams
            , dpi_pingInterval = fromIntegral $ pingInterval addParams
            , dpi_pingTimeout = fromIntegral $ pingTimeout addParams
            , dpi_homogeneous = fromIntegral $ homogeneous addParams
            , dpi_getMode = getMode addParams
            , dpi_timeout = fromIntegral $ timeout addParams
            , dpi_waitTimeout = fromIntegral $ waitTimeout addParams
            , dpi_maxLifetimeSession = fromIntegral $ maxLifetimeSession addParams
            , dpi_maxSessionsPerShard = fromIntegral $ maxSessionsPerShard addParams
            }
        f defaultPoolParmsPtr

foreign import ccall unsafe "dpiPool_create"
  dpiPool_create ::
    -- | const dpiContext *context
    DPIContext ->
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
    -- | uint32_t connLength
    CUInt ->
    -- | const dpiCommonCreateParams *commonParams
    Ptr DPICommonCreateParams ->
    -- | const dpiPoolCreateParams *createParams
    Ptr DPIPoolCreateParams ->
    -- | dpiPool **pool
    Ptr DPIPool ->
    IO CInt

foreign import ccall "&close_pool_default"
  dpiPool_close_finalizer :: FunPtr (Ptr DPIPool -> IO ())

foreign import ccall "&dpiPool_release"
  dpiPool_release_finalizer :: FunPtr (Ptr DPIPool -> IO ())

-- | Close a session pool.
closePool :: Pool -> IO ()
closePool (Pool pool) = finalizeForeignPtr pool

-- | Bracket a computation between creating and closing a session pool.
withPool :: ConnectionParams -> (Pool -> IO c) -> IO c
withPool params = bracket (createPool params) closePool

-- | Acquire a connection from a session pool.
acquireConnection :: Pool -> IO Connection
acquireConnection (Pool poolFptr) = do
  (DPIConn connPtr) <- withForeignPtr poolFptr $ \pool -> do
    alloca $ \conn -> do
      throwOracleError =<< acquire_connection pool conn
      peek conn
  fptr <- newForeignPtr_ connPtr
  addForeignPtrFinalizer dpiConn_release_finalizer fptr
  addForeignPtrFinalizer dpiConn_close_finalizer fptr
  pure (Connection fptr)

foreign import ccall unsafe "acquire_connection"
  acquire_connection ::
    -- | dpiPool *pool
    Ptr DPIPool ->
    -- | dpiConn **conn
    Ptr DPIConn ->
    IO CInt
