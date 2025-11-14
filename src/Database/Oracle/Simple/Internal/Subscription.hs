module Database.Oracle.Simple.Internal.Subscription
  ( subscriptionPrepareStatementDPI,
  )
where

import Foreign (Ptr, alloca, peek, withForeignPtr)
import Foreign.C.String (CString, withCStringLen)
import Foreign.C.Types (CUInt (..))

import Database.Oracle.Simple.Internal.Context (throwOracleError)
import Database.Oracle.Simple.Internal.Entity (DPIStmt, DPISubscription, Subscription)

foreign import ccall "dpiSubscr_prepareStmt"
  dpiSubscr_prepareStmt ::
    Ptr DPISubscription ->
    CString ->
    CUInt ->
    Ptr (Ptr DPIStmt) ->
    IO Int

subscriptionPrepareStatementDPI ::
  Subscription ->
  String ->
  IO (Ptr DPIStmt)
subscriptionPrepareStatementDPI sub sql = do
  withForeignPtr sub $ \subPtr ->
    withCStringLen sql $ \(cSql, cSqlLen) ->
      alloca $ \stmtPtrPtr -> do
        throwOracleError
          =<< dpiSubscr_prepareStmt subPtr cSql (fromIntegral cSqlLen) stmtPtrPtr
        peek stmtPtrPtr
