{-# LANGUAGE ScopedTypeVariables #-}

module Database.Oracle.Simple.Internal.Statement
  ( closeStatement,
    fetch,
    getQueryValue,
    bindValueByPos,
    dpiExecute,
    dpiExecuteMany,
    getRowCount,
    dpiStmt_bindByPos,
  )
where

import Control.Exception.Safe (throwIO)
import Data.Word (Word64)
import Foreign (alloca, peek, poke)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Ptr (Ptr, nullPtr)

import Database.Oracle.Simple.Internal.Context (throwOracleError)
import Database.Oracle.Simple.Internal.Entity
  ( Column (..),
    DPIData,
    DPIModeExec,
    DPINativeType,
    DPIStmt,
    ODPICVar,
    ReadBuffer,
    WriteBuffer,
    maybeFromCUInt,
    toCUInt,
  )

foreign import ccall "dpiStmt_close"
  dpiStmt_close ::
    Ptr DPIStmt ->
    CString ->
    CUInt ->
    IO CInt

closeStatement :: Ptr DPIStmt -> IO ()
closeStatement stmt = throwOracleError =<< dpiStmt_close stmt nullPtr 0

foreign import ccall "dpiStmt_execute"
  dpiStmt_execute ::
    Ptr DPIStmt ->
    CUInt ->
    Ptr CUInt ->
    IO CInt

-- | Execute a statement.
dpiExecute ::
  -- | Statement to be executed
  Ptr DPIStmt ->
  -- | Execution mode
  DPIModeExec ->
  -- | query columns
  IO CUInt
dpiExecute stmt mode =
  alloca $ \rowsPtr -> do
    throwOracleError =<< dpiStmt_execute stmt (toCUInt mode) rowsPtr
    peek rowsPtr

foreign import ccall "dpiStmt_fetch"
  dpiStmt_fetch ::
    Ptr DPIStmt ->
    Ptr CInt ->
    Ptr CUInt ->
    IO CInt

-- | Fetch a single row from the buffers defined for the query.
fetch ::
  -- | Statement from which row is to be fetched
  Ptr DPIStmt ->
  IO CInt
fetch stmt =
  alloca $ \bufferRowIdxPtr ->
    alloca $ \foundPtr -> do
      throwOracleError =<< dpiStmt_fetch stmt foundPtr bufferRowIdxPtr
      peek foundPtr

foreign import ccall "dpiStmt_getQueryValue"
  dpiStmt_getQueryValue ::
    Ptr DPIStmt ->
    CUInt ->
    Ptr CUInt ->
    Ptr (Ptr (DPIData ReadBuffer)) ->
    IO CInt

-- | Return the value of the column at the given position for the currently fetched row.
getQueryValue ::
  -- | Statement from which column value is to be retrieved
  Ptr DPIStmt ->
  -- | Column position
  CUInt ->
  IO (DPINativeType, Ptr (DPIData ReadBuffer))
getQueryValue stmt pos = do
  alloca $ \(buffer :: Ptr (Ptr (DPIData ReadBuffer))) -> do
    alloca $ \(typPtr :: Ptr CUInt) -> do
      throwOracleError =<< dpiStmt_getQueryValue stmt pos typPtr buffer
      mbNativeType <- maybeFromCUInt <$> peek typPtr
      case mbNativeType of
        Nothing ->
          throwIO . userError $ "getQueryValue: Invalid type returned"
        Just nativeType -> do
          dataBuffer <- peek buffer
          pure (nativeType, dataBuffer)

foreign import ccall "dpiStmt_bindValueByPos"
  dpiStmt_bindValueByPos ::
    -- | dpiStmt *stmt
    Ptr DPIStmt ->
    -- | uint32_t pos
    CUInt ->
    -- | dpiNativeTypeNum nativeTypeNum
    CUInt ->
    -- | dpiData *data
    Ptr (DPIData WriteBuffer) ->
    -- | int
    IO CInt

bindValueByPos ::
  Ptr DPIStmt ->
  Column ->
  DPINativeType ->
  DPIData WriteBuffer ->
  IO ()
bindValueByPos stmt col nativeType val = do
  alloca $ \dpiData' -> do
    poke dpiData' val
    throwOracleError
      =<< dpiStmt_bindValueByPos stmt (fromIntegral $ getColumn col) (toCUInt nativeType) dpiData'
    pure ()

foreign import ccall "dpiStmt_getRowCount"
  dpiStmt_getRowCount ::
    Ptr DPIStmt ->
    Ptr Word64 ->
    IO CInt

getRowCount :: Ptr DPIStmt -> IO Word64
getRowCount stmt = do
  alloca $ \rowCount -> do
    throwOracleError =<< dpiStmt_getRowCount stmt rowCount
    peek rowCount

-- Bind a variable to a statement by position
foreign import ccall "dpiStmt_bindByPos"
  dpiStmt_bindByPos ::
    -- | dpiStmt *stmt
    Ptr DPIStmt ->
    -- | uint32_t pos
    CUInt ->
    -- | dpiVar *var
    Ptr ODPICVar ->
    -- | int
    IO CInt

foreign import ccall "dpiStmt_executeMany"
  dpiStmt_executeMany ::
    -- | dpiStmt *stmt
    Ptr DPIStmt ->
    -- | dpiExecMode mode
    CUInt ->
    -- | numIters
    CUInt ->
    -- | int
    IO CInt

-- | Execute a statement.
dpiExecuteMany ::
  -- | Statement to be executed
  Ptr DPIStmt ->
  -- | Execution mode
  DPIModeExec ->
  -- | Array length (iteration count)
  Int ->
  IO ()
dpiExecuteMany stmt mode len =
  throwOracleError =<< dpiStmt_executeMany stmt (toCUInt mode) (fromIntegral len)
