module Database.Oracle.Simple.Internal.Data
  ( dpiData_getIsNull,
    dpiData_getDouble,
    dpiData_getFloat,
    dpiData_getInt64,
    dpiData_getUint64,
    dpiData_getBytes,
    dpiData_getBool,
    dpiData_getTimestamp,
  )
where

import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.Ptr (Ptr)

import Database.Oracle.Simple.Internal.Entity (DPIBytes, DPIData, ReadBuffer)
import Database.Oracle.Simple.Internal.Timestamp (DPITimestamp)

foreign import ccall "dpiData_getDouble"
  dpiData_getDouble :: Ptr (DPIData ReadBuffer) -> IO Double

foreign import ccall "dpiData_getFloat"
  dpiData_getFloat :: Ptr (DPIData ReadBuffer) -> IO Float

foreign import ccall "dpiData_getBytes"
  dpiData_getBytes :: Ptr (DPIData ReadBuffer) -> IO (Ptr DPIBytes)

foreign import ccall "dpiData_getTimestamp"
  dpiData_getTimestamp :: Ptr (DPIData ReadBuffer) -> IO (Ptr DPITimestamp)

foreign import ccall "dpiData_getInt64"
  dpiData_getInt64 :: Ptr (DPIData ReadBuffer) -> IO Int64

foreign import ccall "dpiData_getUint64"
  dpiData_getUint64 :: Ptr (DPIData ReadBuffer) -> IO Word64

foreign import ccall "dpiData_getBool"
  dpiData_getBool :: Ptr (DPIData ReadBuffer) -> IO Int

foreign import ccall "dpiData_getIsNull"
  dpiData_getIsNull :: Ptr (DPIData ReadBuffer) -> IO Int
