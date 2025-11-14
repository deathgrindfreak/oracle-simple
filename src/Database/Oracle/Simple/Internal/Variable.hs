module Database.Oracle.Simple.Internal.Variable
  ( dpiVar_getNumElementsInArray,
    dpiVar_setNumElementsInArray,
    dpiVar_getSizeInBytes,
    dpiVar_setFromBytes,
    dpiVar_release,
  )
where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Ptr (FunPtr, Ptr)

import Database.Oracle.Simple.Internal.Entity (ODPICVar)

foreign import ccall "&dpiVar_release"
  dpiVar_release :: FunPtr (Ptr ODPICVar -> IO ())

foreign import ccall "dpiVar_getNumElementsInArray"
  dpiVar_getNumElementsInArray ::
    -- | dpiVar *var
    Ptr ODPICVar ->
    -- | uint32_t *numElements (OUT)
    Ptr CUInt ->
    -- | int
    IO CInt

foreign import ccall "dpiVar_setNumElementsInArray"
  dpiVar_setNumElementsInArray ::
    -- | dpiVar *var
    Ptr ODPICVar ->
    -- | uint32_t numElements
    CUInt ->
    -- | int
    IO CInt

foreign import ccall "dpiVar_getSizeInBytes"
  dpiVar_getSizeInBytes ::
    -- | dpiVar *var
    Ptr ODPICVar ->
    -- | uint32_t *sizeInBytes
    Ptr CUInt ->
    -- | int
    IO CInt

foreign import ccall "dpiVar_setFromBytes"
  dpiVar_setFromBytes ::
    -- | dpiVar *var
    Ptr ODPICVar ->
    -- | uint32_t pos
    CUInt ->
    -- | const char *value
    CString ->
    -- | uint32_t valueLength
    CUInt ->
    -- | int
    IO CInt
