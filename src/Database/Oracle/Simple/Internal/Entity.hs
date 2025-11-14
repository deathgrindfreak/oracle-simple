{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Avoid restricted function" -}

module Database.Oracle.Simple.Internal.Entity
  ( SqlStatement,
    DPINativeType (..),
    DPIData (..),
    DPIBytes (..),
    DPIStmt,
    DPIModeExec (..),
    DPIConn,
    Connection,
    DPISubscription,
    Subscription,
    DPIContext,
    DPITimestamp (..),
    DPIOracleType (..),
    DPICommonCreateParams (..),
    CommonCreateParams (..),
    createModeToDPIFlags,
    CreateMode (..),
    DPIPoolCreateParams (..),
    PoolCreateParams (..),
    DPIConnectionCreateParams (..),
    DPIPool,
    WriteBuffer (..),
    ReadBuffer (..),
    Column (..),
    Only (..),
    CEnum (..),
    ConnectionParams (..),
    DPISubscriptionCreateParams (..),
    DPISubscriptionNamespace (..),
    DPISubscriptionProtocol (..),
    DPISubscriptionQOS (..),
    QOS (..),
    toDPIQOS,
    DPIOpCode (..),
    DPIEventType (..),
    DPISubscriptionMessageRow (..),
    DPISubscriptionMessageTable (..),
    DPISubscriptionMessageQuery (..),
    DPISubscriberMessage (..),
    DPISubscrGroupingClass (..),
    SubscriptionOperationNotifications (..),
    SubscriptionCreateOptions (..),
    SubscriptionOperationNotification (..),
    subscriptionOperationNotificationsToFlags,
    fromOpCodes,
    CFlags,
    toFlags,
    fromFlags,
    SubscriberMessage,
    toSubscriberMessage,
    mkSubscriptionCallback,
    SubscriptionCallback,
    OracleError (..),
    ErrorInfo (..),
    VersionInfo (..),
    ODPICData,
    ODPICVar,
    mkDPIBytesUTF8,
  )
where

import Control.Exception.Safe (Exception)
import Data.Bits ((.&.), (.|.))
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.String (CString, newCString, newCStringLen)
import Foreign.C.Types (CBool, CInt (..), CUChar (..), CUInt (..), CULong (..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable.Generic (GStorable, Storable (..))
import GHC.Generics (Generic)
import GHC.TypeLits (Natural)

import Database.Oracle.Simple.Internal.DPIEnum (CEnum (..), CEnumChar (..), DPIEnum (..), DPIEnumChar (..))
import Database.Oracle.Simple.Internal.Timestamp (DPITimestamp (..))

type SqlStatement = String

data DPIConn
type Connection = ForeignPtr DPIConn

data DPIStmt

data DPIPool

data DPIShardingKeyColumn

data DPISubscription
type Subscription = ForeignPtr DPISubscription

data DPIContext

data ConnectionParams = ConnectionParams
  { user :: String
  , pass :: String
  , connString :: String
  , commonCreateParams :: Maybe CommonCreateParams
  , createPoolParams :: Maybe PoolCreateParams
  }
  deriving (Eq, Show)

-- | typedef uint8_t dpiPoolGetMode;
data DPIPoolGetMode
  = DPI_MODE_POOL_GET_FORCEGET
  | DPI_MODE_POOL_GET_NOWAIT
  | DPI_MODE_POOL_GET_TIMEDWAIT
  | DPI_MODE_POOL_GET_WAIT
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, CEnumChar)
  deriving (Storable) via (DPIEnumChar DPIPoolGetMode)

-- | typedef uint32_t dpiCreateMode;
data DPICreateMode
  = DPI_MODE_CREATE_DEFAULT --  0x00000000
  | DPI_MODE_CREATE_THREADED -- 0x00000001
  | DPI_MODE_CREATE_EVENTS --   0x00000004
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnum DPICreateMode)

instance CEnum DPICreateMode where
  toCUInt DPI_MODE_CREATE_DEFAULT = 0x00000000
  toCUInt DPI_MODE_CREATE_THREADED = 0x00000001
  toCUInt DPI_MODE_CREATE_EVENTS = 0x00000004

data DPIPoolCreateParams = DPIPoolCreateParams
  { dpi_minSessions :: CUInt
  , dpi_maxSessions :: CUInt
  , dpi_sessionIncrement :: CUInt
  , dpi_pingInterval :: CInt
  , dpi_pingTimeout :: CInt
  , dpi_homogeneous :: CInt
  , dpi_externalAuth :: CInt
  , dpi_getMode :: DPIPoolGetMode
  , dpi_outPoolName :: CString
  , dpi_outPoolNameLength :: CUInt
  , dpi_timeout :: CUInt
  , dpi_waitTimeout :: CUInt
  , dpi_maxLifetimeSession :: CUInt
  , dpi_plsqlFixupCallback :: CString
  , dpi_plsqlFixupCallbackLength :: CUInt
  , dpi_maxSessionsPerShard :: CUInt
  , dpi_accessTokenCallback :: FunPtr ()
  , dpi_accessTokenCallbackContext :: Ptr ()
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data PoolCreateParams = PoolCreateParams
  { minSessions :: Natural
  , maxSessions :: Natural
  , sessionIncrement :: Natural
  , pingInterval :: Natural
  , pingTimeout :: Natural
  , homogeneous :: Natural
  , getMode :: DPIPoolGetMode
  , timeout :: Natural
  , waitTimeout :: Natural
  , maxLifetimeSession :: Natural
  , maxSessionsPerShard :: Natural
  }
  deriving (Eq, Ord, Show)

data DPICommonCreateParams = DPICommonCreateParams
  { dpi_createMode :: CFlags DPICreateMode
  , dpi_encoding :: CString
  , dpi_nencoding :: CString
  , dpi_edition :: CString
  , dpi_editionLength :: CUInt
  , dpi_driverName :: CString
  , dpi_driverNameLength :: CUInt
  , dpi_sodaMetadataCache :: Int
  , dpi_stmtCacheSize :: CUInt
  , dpi_accessToken :: Ptr ()
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data CreateMode = DefaultCreateMode | EventsCreateMode | ThreadsCreateMode
  deriving (Show, Eq, Ord, Enum, Bounded)

createModeToDPIFlag :: CreateMode -> DPICreateMode
createModeToDPIFlag mode =
  case mode of
    DefaultCreateMode -> DPI_MODE_CREATE_DEFAULT
    ThreadsCreateMode -> DPI_MODE_CREATE_THREADED
    EventsCreateMode -> DPI_MODE_CREATE_EVENTS

createModeToDPIFlags :: Set.Set CreateMode -> CFlags DPICreateMode
createModeToDPIFlags =
  CFlags . foldr (\m a -> a .|. (toCUInt . createModeToDPIFlag) m) 0

data CommonCreateParams = CommonCreateParams
  { createMode :: Set.Set CreateMode
  , encoding :: String
  , nencoding :: String
  , edition :: String
  , driverName :: String
  , sodaMetadataCache :: Int
  , stmtCacheSize :: Int
  }
  deriving (Eq, Show)

data VersionInfo = VersionInfo
  { versionNum :: CInt
  , releaseNum :: CInt
  , updateNum :: CInt
  , portReleaseNum :: CInt
  , portUpdateNum :: CInt
  , fullVersionNum :: CUInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data DPIContextCreateParams = DPIContextCreateParams
  { defaultDriverName :: CString
  , defaultEncoding :: CString
  , loadErrorUrl :: CString
  , oracleClientLibDir :: CString
  , oracleClientConfigDir :: CString
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

-- typedef uint32_t dpiPurity;
data DPIPurity
  = DPI_PURITY_DEFAULT
  | DPI_PURITY_NEW
  | DPI_PURITY_SELF
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, CEnum)
  deriving (Storable) via (DPIEnum DPIPurity)

data DPIModeConnClose
  = DPI_MODE_CONN_CLOSE_DEFAULT -- 0x0000
  | DPI_MODE_CONN_CLOSE_DROP -- 0x0001
  | DPI_MODE_CONN_CLOSE_RETAG -- 0x0002
  deriving (Show, Eq, Enum, Bounded, CEnum)
  deriving (Storable) via (DPIEnum DPIModeConnClose)

data DPIConnectionCreateParams = DPIConnectionCreateParams
  { authMode :: DPIAuthMode
  , connectionClass :: CString
  , connectionClassLength :: CUInt
  , purity :: DPIPurity
  , newPassword :: CString
  , newPasswordLength :: CUInt
  , appContenxt :: DPIAppContext
  , numAppContext :: CUInt
  , externalAuth :: CInt
  , externalHandle :: Ptr ()
  , pool :: Ptr DPIPool
  , tag :: CString
  , tagLength :: CUInt
  , matchAnyTag :: CInt
  , outTag :: CString
  , outTagLength :: CUInt
  , outTagFound :: CInt
  , shardingKeyColumn :: Ptr DPIShardingKeyColumn
  , numShardingKeyColumns :: Word8
  , superShardingKeyColumns :: Ptr DPIShardingKeyColumn
  , numSuperShardingKeyColumns :: Word8
  , outNewSession :: CInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

-- | typedef uint32_t dpiAuthMode;
data DPIAuthMode
  = DPI_MODE_AUTH_DEFAULT -- 0x00000000
  | DPI_MODE_AUTH_SYSDBA -- 0x00000002
  | DPI_MODE_AUTH_SYSOPER -- 0x00000004
  | DPI_MODE_AUTH_PRELIM -- 0x00000008
  | DPI_MODE_AUTH_SYSASM -- 0x00008000
  | DPI_MODE_AUTH_SYSBKP -- 0x00020000
  | DPI_MODE_AUTH_SYSDGD -- 0x00040000
  | DPI_MODE_AUTH_SYSKMT -- 0x00080000
  | DPI_MODE_AUTH_SYSRAC -- 0x00100000
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnum DPIAuthMode)

instance CEnum DPIAuthMode where
  toCUInt DPI_MODE_AUTH_DEFAULT = 0x00000000
  toCUInt DPI_MODE_AUTH_SYSDBA = 0x00000002
  toCUInt DPI_MODE_AUTH_SYSOPER = 0x00000004
  toCUInt DPI_MODE_AUTH_PRELIM = 0x00000008
  toCUInt DPI_MODE_AUTH_SYSASM = 0x00008000
  toCUInt DPI_MODE_AUTH_SYSBKP = 0x00020000
  toCUInt DPI_MODE_AUTH_SYSDGD = 0x00040000
  toCUInt DPI_MODE_AUTH_SYSKMT = 0x00080000
  toCUInt DPI_MODE_AUTH_SYSRAC = 0x00100000

data DPIBytes = DPIBytes
  { dpiBytesPtr :: CString
  , dpiBytesLength :: CUInt
  , dpiBytesEncoding :: CString
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

mkDPIBytesUTF8 :: String -> IO DPIBytes
mkDPIBytesUTF8 str = do
  (dpiBytesPtr, fromIntegral -> dpiBytesLength) <- newCStringLen str
  dpiBytesEncoding <- newCString "UTF-8"
  pure $ DPIBytes {..}

data DPIAppContext = DPIAppContext
  { namespaceName :: CString
  , namespaceNameLength :: CUInt
  , name :: CString
  , nameLength :: CUInt
  , value :: CString
  , valueLength :: CUInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data DPIModeExec
  = DPI_MODE_EXEC_DEFAULT -- 0x00000000
  | DPI_MODE_EXEC_DESCRIBE_ONLY -- 0x00000010
  | DPI_MODE_EXEC_COMMIT_ON_SUCCESS -- 0x00000020
  | DPI_MODE_EXEC_BATCH_ERRORS -- 0x00000080
  | DPI_MODE_EXEC_PARSE_ONLY -- 0x00000100
  | DPI_MODE_EXEC_ARRAY_DML_ROWCOUNTS -- 0x00100000
  deriving (Show, Eq, Ord, Enum, Bounded)
  deriving (Storable) via (DPIEnum DPIModeExec)

instance CEnum DPIModeExec where
  toCUInt DPI_MODE_EXEC_DEFAULT = 0x00000000
  toCUInt DPI_MODE_EXEC_DESCRIBE_ONLY = 0x00000010
  toCUInt DPI_MODE_EXEC_COMMIT_ON_SUCCESS = 0x00000020
  toCUInt DPI_MODE_EXEC_BATCH_ERRORS = 0x00000080
  toCUInt DPI_MODE_EXEC_PARSE_ONLY = 0x00000100
  toCUInt DPI_MODE_EXEC_ARRAY_DML_ROWCOUNTS = 0x00100000

data DPINativeType
  = DPI_NATIVE_TYPE_INT64
  | DPI_NATIVE_TYPE_UINT64
  | DPI_NATIVE_TYPE_FLOAT
  | DPI_NATIVE_TYPE_DOUBLE
  | DPI_NATIVE_TYPE_BYTES
  | DPI_NATIVE_TYPE_TIMESTAMP
  | DPI_NATIVE_TYPE_INTERVAL_DS
  | DPI_NATIVE_TYPE_INTERVAL_YM
  | DPI_NATIVE_TYPE_LOB
  | DPI_NATIVE_TYPE_OBJECT
  | DPI_NATIVE_TYPE_STMT
  | DPI_NATIVE_TYPE_BOOLEAN
  | DPI_NATIVE_TYPE_ROWID
  | DPI_NATIVE_TYPE_JSON
  | DPI_NATIVE_TYPE_JSON_OBJECT
  | DPI_NATIVE_TYPE_JSON_ARRAY
  | DPI_NATIVE_TYPE_NULL
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnum DPINativeType)

instance CEnum DPINativeType where
  toCUInt DPI_NATIVE_TYPE_INT64 = 3000
  toCUInt DPI_NATIVE_TYPE_UINT64 = 3001
  toCUInt DPI_NATIVE_TYPE_FLOAT = 3002
  toCUInt DPI_NATIVE_TYPE_DOUBLE = 3003
  toCUInt DPI_NATIVE_TYPE_BYTES = 3004
  toCUInt DPI_NATIVE_TYPE_TIMESTAMP = 3005
  toCUInt DPI_NATIVE_TYPE_INTERVAL_DS = 3006
  toCUInt DPI_NATIVE_TYPE_INTERVAL_YM = 3007
  toCUInt DPI_NATIVE_TYPE_LOB = 3008
  toCUInt DPI_NATIVE_TYPE_OBJECT = 3009
  toCUInt DPI_NATIVE_TYPE_STMT = 3010
  toCUInt DPI_NATIVE_TYPE_BOOLEAN = 3011
  toCUInt DPI_NATIVE_TYPE_ROWID = 3012
  toCUInt DPI_NATIVE_TYPE_JSON = 3013
  toCUInt DPI_NATIVE_TYPE_JSON_OBJECT = 3014
  toCUInt DPI_NATIVE_TYPE_JSON_ARRAY = 3015
  toCUInt DPI_NATIVE_TYPE_NULL = 3016

{- | Oracle data types.
Includes types used for columns in tables as well as types exclusive to PL/SQL.
Each type maps to a DPI native type to read/write values via ODPI functions.
-}
data DPIOracleType
  = DPI_ORACLE_TYPE_NONE
  | DPI_ORACLE_TYPE_VARCHAR
  | DPI_ORACLE_TYPE_NVARCHAR
  | DPI_ORACLE_TYPE_CHAR
  | DPI_ORACLE_TYPE_NCHAR
  | DPI_ORACLE_TYPE_ROWID
  | DPI_ORACLE_TYPE_RAW
  | DPI_ORACLE_TYPE_NATIVE_FLOAT
  | DPI_ORACLE_TYPE_NATIVE_DOUBLE
  | DPI_ORACLE_TYPE_NATIVE_INT
  | DPI_ORACLE_TYPE_NUMBER
  | DPI_ORACLE_TYPE_DATE
  | DPI_ORACLE_TYPE_TIMESTAMP
  | DPI_ORACLE_TYPE_TIMESTAMP_TZ
  | DPI_ORACLE_TYPE_TIMESTAMP_LTZ
  | DPI_ORACLE_TYPE_INTERVAL_DS
  | DPI_ORACLE_TYPE_INTERVAL_YM
  | DPI_ORACLE_TYPE_CLOB
  | DPI_ORACLE_TYPE_NCLOB
  | DPI_ORACLE_TYPE_BLOB
  | DPI_ORACLE_TYPE_BFILE
  | DPI_ORACLE_TYPE_STMT
  | DPI_ORACLE_TYPE_BOOLEAN
  | DPI_ORACLE_TYPE_OBJECT
  | DPI_ORACLE_TYPE_LONG_VARCHAR
  | DPI_ORACLE_TYPE_LONG_RAW
  | DPI_ORACLE_TYPE_NATIVE_UINT
  | DPI_ORACLE_TYPE_JSON
  | DPI_ORACLE_TYPE_JSON_OBJECT
  | DPI_ORACLE_TYPE_JSON_ARRAY
  | DPI_ORACLE_TYPE_UROWID
  | DPI_ORACLE_TYPE_LONG_NVARCHAR
  | DPI_ORACLE_TYPE_MAX
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnum DPIOracleType)

instance CEnum DPIOracleType where
  toCUInt DPI_ORACLE_TYPE_NONE = 2000
  toCUInt DPI_ORACLE_TYPE_VARCHAR = 2001
  toCUInt DPI_ORACLE_TYPE_NVARCHAR = 2002
  toCUInt DPI_ORACLE_TYPE_CHAR = 2003
  toCUInt DPI_ORACLE_TYPE_NCHAR = 2004
  toCUInt DPI_ORACLE_TYPE_ROWID = 2005
  toCUInt DPI_ORACLE_TYPE_RAW = 2006
  toCUInt DPI_ORACLE_TYPE_NATIVE_FLOAT = 2007
  toCUInt DPI_ORACLE_TYPE_NATIVE_DOUBLE = 2008
  toCUInt DPI_ORACLE_TYPE_NATIVE_INT = 2009
  toCUInt DPI_ORACLE_TYPE_NUMBER = 2010
  toCUInt DPI_ORACLE_TYPE_DATE = 2011
  toCUInt DPI_ORACLE_TYPE_TIMESTAMP = 2012
  toCUInt DPI_ORACLE_TYPE_TIMESTAMP_TZ = 2013
  toCUInt DPI_ORACLE_TYPE_TIMESTAMP_LTZ = 2014
  toCUInt DPI_ORACLE_TYPE_INTERVAL_DS = 2015
  toCUInt DPI_ORACLE_TYPE_INTERVAL_YM = 2016
  toCUInt DPI_ORACLE_TYPE_CLOB = 2017
  toCUInt DPI_ORACLE_TYPE_NCLOB = 2018
  toCUInt DPI_ORACLE_TYPE_BLOB = 2019
  toCUInt DPI_ORACLE_TYPE_BFILE = 2020
  toCUInt DPI_ORACLE_TYPE_STMT = 2021
  toCUInt DPI_ORACLE_TYPE_BOOLEAN = 2022
  toCUInt DPI_ORACLE_TYPE_OBJECT = 2023
  toCUInt DPI_ORACLE_TYPE_LONG_VARCHAR = 2024
  toCUInt DPI_ORACLE_TYPE_LONG_RAW = 2025
  toCUInt DPI_ORACLE_TYPE_NATIVE_UINT = 2026
  toCUInt DPI_ORACLE_TYPE_JSON = 2027
  toCUInt DPI_ORACLE_TYPE_JSON_OBJECT = 2028
  toCUInt DPI_ORACLE_TYPE_JSON_ARRAY = 2029
  toCUInt DPI_ORACLE_TYPE_UROWID = 2030
  toCUInt DPI_ORACLE_TYPE_LONG_NVARCHAR = 2031
  toCUInt DPI_ORACLE_TYPE_MAX = 2032

data DPISubscriptionNamespace
  = DPI_SUBSCR_NAMESPACE_AQ
  | DPI_SUBSCR_NAMESPACE_DBCHANGE
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnum DPISubscriptionNamespace)

instance CEnum DPISubscriptionNamespace where
  toCUInt DPI_SUBSCR_NAMESPACE_AQ = 1
  toCUInt DPI_SUBSCR_NAMESPACE_DBCHANGE = 2

data DPISubscriptionProtocol
  = DPI_SUBSCR_PROTO_CALLBACK
  | DPI_SUBSCR_PROTO_MAIL
  | DPI_SUBSCR_PROTO_PLSQL
  | DPI_SUBSCR_PROTO_HTTP
  deriving (Show, Eq, Enum, Bounded, CEnum)
  deriving (Storable) via (DPIEnum DPISubscriptionProtocol)

data DPISubscriptionQOS
  = DPI_SUBSCR_QOS_BEST_EFFORT
  | DPI_SUBSCR_QOS_DEREG_NFY
  | DPI_SUBSCR_QOS_QUERY
  | DPI_SUBSCR_QOS_RELIABLE
  | DPI_SUBSCR_QOS_ROWIDS
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnum DPISubscriptionQOS)

instance CEnum DPISubscriptionQOS where
  toCUInt DPI_SUBSCR_QOS_RELIABLE = 0x01
  toCUInt DPI_SUBSCR_QOS_DEREG_NFY = 0x02
  toCUInt DPI_SUBSCR_QOS_ROWIDS = 0x04
  toCUInt DPI_SUBSCR_QOS_QUERY = 0x08
  toCUInt DPI_SUBSCR_QOS_BEST_EFFORT = 0x10

data QOS
  = QOSBestEffort
  | QOSDeregNotify
  | QOSQuery
  | QOSReliable
  | QOSRowIds
  deriving (Show, Eq, Ord)

toDPIQOS :: QOS -> DPISubscriptionQOS
toDPIQOS qos =
  case qos of
    QOSBestEffort -> DPI_SUBSCR_QOS_BEST_EFFORT
    QOSDeregNotify -> DPI_SUBSCR_QOS_DEREG_NFY
    QOSQuery -> DPI_SUBSCR_QOS_QUERY
    QOSReliable -> DPI_SUBSCR_QOS_RELIABLE
    QOSRowIds -> DPI_SUBSCR_QOS_ROWIDS

data DPIOpCode
  = DPI_OPCODE_ALL_OPS
  | DPI_OPCODE_ALL_ROWS
  | DPI_OPCODE_ALTER
  | DPI_OPCODE_DELETE
  | DPI_OPCODE_DROP
  | DPI_OPCODE_INSERT
  | DPI_OPCODE_UPDATE
  | DPI_OPCODE_UNKNOWN
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnum DPIOpCode)

instance CEnum DPIOpCode where
  toCUInt DPI_OPCODE_ALL_OPS = 0x0
  toCUInt DPI_OPCODE_ALL_ROWS = 0x1
  toCUInt DPI_OPCODE_INSERT = 0x2
  toCUInt DPI_OPCODE_UPDATE = 0x4
  toCUInt DPI_OPCODE_DELETE = 0x8
  toCUInt DPI_OPCODE_ALTER = 0x10
  toCUInt DPI_OPCODE_DROP = 0x20
  toCUInt DPI_OPCODE_UNKNOWN = 0x40

data SubscriptionOperationNotifications
  = AllNotifications
  | Notifications (Set.Set SubscriptionOperationNotification)
  deriving (Show, Eq)

data SubscriptionOperationNotification
  = AllRowsNotification
  | InsertNotification
  | UpdateNotification
  | DeleteNotification
  | AlterNotification
  | DropNotification
  | UnknownNotification
  deriving (Show, Eq, Ord, Enum, Bounded)

toDPIOpCode :: SubscriptionOperationNotification -> DPIOpCode
toDPIOpCode notifications =
  case notifications of
    AllRowsNotification -> DPI_OPCODE_ALL_ROWS
    InsertNotification -> DPI_OPCODE_INSERT
    UpdateNotification -> DPI_OPCODE_UPDATE
    DeleteNotification -> DPI_OPCODE_DELETE
    AlterNotification -> DPI_OPCODE_ALTER
    DropNotification -> DPI_OPCODE_DROP
    UnknownNotification -> DPI_OPCODE_UNKNOWN

newtype CFlags rep = CFlags CUInt
  deriving newtype (Show, Eq, Storable)

toFlags :: CEnum b => (a -> b) -> Set.Set a -> CFlags b
toFlags f = CFlags . foldr (\n a -> a .|. (toCUInt . f) n) 0

fromFlags :: (Enum a, Bounded a, Ord a, CEnum b) => (a -> b) -> CFlags b -> Set.Set a
fromFlags f (CFlags flags) =
  Set.fromList $
    mapMaybe
      ( \n ->
          if (flags .&. toCUInt (f n)) == 0
            then Nothing
            else Just n
      )
      [minBound .. maxBound]

subscriptionOperationNotificationsToFlags ::
  SubscriptionOperationNotifications ->
  CFlags DPIOpCode
subscriptionOperationNotificationsToFlags notifications =
  case notifications of
    AllNotifications -> CFlags 0
    Notifications notificationSet -> toFlags toDPIOpCode notificationSet

fromOpCodes ::
  CFlags DPIOpCode ->
  SubscriptionOperationNotifications
fromOpCodes cFlags =
  case cFlags of
    CFlags 0 -> AllNotifications
    flags -> Notifications $ fromFlags toDPIOpCode flags

data DPIEventType
  = DPI_EVENT_NONE
  | DPI_EVENT_STARTUP
  | DPI_EVENT_SHUTDOWN
  | DPI_EVENT_SHUTDOWN_ANY
  | DPI_EVENT_DEREG
  | DPI_EVENT_OBJCHANGE
  | DPI_EVENT_QUERYCHANGE
  | DPI_EVENT_AQ
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnum DPIEventType)

instance CEnum DPIEventType where
  toCUInt DPI_EVENT_NONE = 0
  toCUInt DPI_EVENT_STARTUP = 1
  toCUInt DPI_EVENT_SHUTDOWN = 2
  toCUInt DPI_EVENT_SHUTDOWN_ANY = 3
  toCUInt DPI_EVENT_DEREG = 5
  toCUInt DPI_EVENT_OBJCHANGE = 6
  toCUInt DPI_EVENT_QUERYCHANGE = 7
  toCUInt DPI_EVENT_AQ = 100

data EventType
  = EventNone
  | EventStartup
  | EventShutdown
  | EventShutdownAny
  | EventDereg
  | EventObjChange
  | EventQueryChange
  | EventAQ
  deriving (Show, Eq)

fromDPIEventType :: DPIEventType -> EventType
fromDPIEventType dpiET =
  case dpiET of
    DPI_EVENT_NONE -> EventNone
    DPI_EVENT_STARTUP -> EventStartup
    DPI_EVENT_SHUTDOWN -> EventShutdown
    DPI_EVENT_SHUTDOWN_ANY -> EventShutdownAny
    DPI_EVENT_DEREG -> EventDereg
    DPI_EVENT_OBJCHANGE -> EventObjChange
    DPI_EVENT_QUERYCHANGE -> EventQueryChange
    DPI_EVENT_AQ -> EventAQ

data DPISubscriptionMessageRow = DPISubscriptionMessageRow
  { dpiSMR_operation :: CFlags DPIOpCode
  , dpiSMR_rowid :: CString
  , dpiSMR_rowidLength :: CUInt
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GStorable)

data DPISubscriptionMessageTable = DPISubscriptionMessageTable
  { dpiSMT_operation :: CFlags DPIOpCode
  , dpiSMT_name :: CString
  , dpiSMT_nameLength :: CUInt
  , dpiSMT_rows :: Ptr DPISubscriptionMessageRow
  , dpiSMT_numRows :: CUInt
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GStorable)

data DPISubscriptionMessageQuery = DPISubscriptionMessageQuery
  { dpiSMQ_id :: CULong
  , dpiSMQ_operation :: CFlags DPIOpCode
  , dpiSMQ_tables :: Ptr DPISubscriptionMessageTable
  , dpiSMQ_numTables :: CUInt
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GStorable)

data DPISubscriberMessage = DPISubscriberMessage
  { dpiSM_eventType :: DPIEventType
  , dpiSM_dbName :: CString
  , dpiSM_dbNameLength :: CUInt
  , dpiSM_tables :: Ptr DPISubscriptionMessageTable
  , dpiSM_numTables :: CUInt
  , dpiSM_queries :: Ptr DPISubscriptionMessageQuery
  , dpiSM_numQueries :: CUInt
  , dpiSM_errorInfo :: Ptr ErrorInfo
  , dpiSM_txId :: Ptr ()
  , dpiSM_txIdLength :: CUInt
  , dpiSM_registered :: CBool
  , dpiSM_queueName :: CString
  , dpiSM_queueNameLength :: CUInt
  , dpiSM_consumerName :: CString
  , dpiSM_consumerNameLength :: CUInt
  , dpiSM_aqMsgId :: Ptr ()
  , dpiSM_aqMsgIdLength :: CUInt
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GStorable)

-- | typedef uint8_t dpiSubscrGroupingClass;
data DPISubscrGroupingClass
  = DPI_SUBSCR_GROUPING_CLASS_TIME
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnumChar DPISubscrGroupingClass)

instance CEnumChar DPISubscrGroupingClass where
  toCUChar DPI_SUBSCR_GROUPING_CLASS_TIME = 1

-- | typedef uint8_t dpiSubscrGroupingType;
data DPISubscrGroupingType
  = DPI_SUBSCR_GROUPING_TYPE_SUMMARY
  | DPI_SUBSCR_GROUPING_TYPE_LAST
  deriving (Show, Eq, Enum, Bounded)
  deriving (Storable) via (DPIEnumChar DPISubscrGroupingType)

instance CEnumChar DPISubscrGroupingType where
  toCUChar DPI_SUBSCR_GROUPING_TYPE_SUMMARY = 1
  toCUChar DPI_SUBSCR_GROUPING_TYPE_LAST = 2

data SubscriptionMessageRow = SubscriptionMessageRow
  { rowOperations :: SubscriptionOperationNotifications
  , rowId :: T.Text
  }
  deriving (Show, Eq)

data SubscriptionMessageQuery = SubscriptionMessageQuery
  { queryID :: Int64
  , queryOperations :: SubscriptionOperationNotifications
  , queryTables :: [SubscriptionMessageTable]
  }
  deriving (Show, Eq)

data SubscriptionMessageTable = SubscriptionMessageTable
  { tableOperations :: SubscriptionOperationNotifications
  , tableName :: T.Text
  , tableRows :: [SubscriptionMessageRow]
  }
  deriving (Show, Eq)

data SubscriberMessage = SubscriberMessage
  { eventType :: EventType
  , dbName :: T.Text
  , tables :: [SubscriptionMessageTable]
  , queries :: [SubscriptionMessageQuery]
  , errorInfo :: Maybe ErrorInfo
  , registered :: Bool
  , queueName :: T.Text
  , consumerName :: T.Text
  }
  deriving (Show, Eq)

toSubscriptionMessageRow :: DPISubscriptionMessageRow -> IO SubscriptionMessageRow
toSubscriptionMessageRow DPISubscriptionMessageRow {..} = do
  rowId <- TF.peekCStringLen (dpiSMR_rowid, fromIntegral dpiSMR_rowidLength)
  pure
    SubscriptionMessageRow
      { rowOperations = fromOpCodes dpiSMR_operation
      , rowId
      }
toSubscriptionMessageQuery :: DPISubscriptionMessageQuery -> IO SubscriptionMessageQuery
toSubscriptionMessageQuery DPISubscriptionMessageQuery {..} = do
  queryTables <- traverse toSubscriptionMessageTable =<< peekArray (fromIntegral dpiSMQ_numTables) dpiSMQ_tables
  pure
    SubscriptionMessageQuery
      { queryID = fromIntegral dpiSMQ_id
      , queryOperations = fromOpCodes dpiSMQ_operation
      , queryTables
      }

toSubscriptionMessageTable :: DPISubscriptionMessageTable -> IO SubscriptionMessageTable
toSubscriptionMessageTable DPISubscriptionMessageTable {..} = do
  tableName <- TF.peekCStringLen (dpiSMT_name, fromIntegral dpiSMT_nameLength)
  tableRows <- traverse toSubscriptionMessageRow =<< peekArray (fromIntegral dpiSMT_numRows) dpiSMT_rows
  pure
    SubscriptionMessageTable
      { tableOperations = fromOpCodes dpiSMT_operation
      , tableName
      , tableRows
      }

toSubscriberMessage :: DPISubscriberMessage -> IO SubscriberMessage
toSubscriberMessage DPISubscriberMessage {..} = do
  dbName <- TF.peekCStringLen (dpiSM_dbName, fromIntegral dpiSM_dbNameLength)
  tables <- traverse toSubscriptionMessageTable =<< peekArray (fromIntegral dpiSM_numTables) dpiSM_tables
  queries <- traverse toSubscriptionMessageQuery =<< peekArray (fromIntegral dpiSM_numQueries) dpiSM_queries
  errorInfo <-
    if nullPtr == dpiSM_errorInfo
      then pure Nothing
      else Just <$> peek dpiSM_errorInfo
  queueName <- TF.peekCStringLen (dpiSM_queueName, fromIntegral dpiSM_queueNameLength)
  consumerName <- TF.peekCStringLen (dpiSM_consumerName, fromIntegral dpiSM_consumerNameLength)
  pure
    SubscriberMessage
      { eventType = fromDPIEventType dpiSM_eventType
      , dbName
      , tables
      , queries
      , errorInfo
      , registered = toBool dpiSM_registered
      , queueName
      , consumerName
      }

type SubscriptionCallback = Ptr () -> Ptr DPISubscriberMessage -> IO ()
foreign import ccall "wrapper"
  mkSubscriptionCallback :: SubscriptionCallback -> IO (FunPtr SubscriptionCallback)

data DPISubscriptionCreateParams = DPISubscriptionCreateParams
  { dpi_subscrNamespace :: DPISubscriptionNamespace
  , dpi_subscrProtocol :: DPISubscriptionProtocol
  , dpi_qos :: CFlags DPISubscriptionQOS
  , dpi_operations :: CFlags DPIOpCode
  , dpi_portNumber :: CUInt
  , dpi_subscriptionTimeout :: CUInt
  , dpi_name :: CString
  , dpi_nameLength :: CUInt
  , dpi_callback :: FunPtr SubscriptionCallback
  , dpi_callbackContext :: Ptr ()
  , dpi_recipientName :: CString
  , dpi_recipientNameLength :: CUInt
  , dpi_ipAddress :: CString
  , dpi_ipAddressLength :: CUInt
  , -- Should be set to DPISubscrGroupingClass, however the default value is technically 0,
    -- but technically the only valid allowed here is 1.  Thus I'm leaving it as a bare CUChar and setting
    -- it the places where it seg faults 😑
    dpi_groupingClass :: CUChar
  , dpi_groupingValue :: CUInt
  , dpi_groupingType :: DPISubscrGroupingType
  , dpi_outRegId :: CULong
  , dpi_clientInitiated :: CBool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GStorable)

data SubscriptionCreateOptions = SubscriptionCreateOptions
  { operations :: SubscriptionOperationNotifications
  , qos :: Set.Set QOS
  }
  deriving (Show, Eq)

-- | Used to write values to or read values from a column.
data DPIData a = DPIData
  { dataIsNull :: CInt
  -- ^ If reading, a null value was read. If writing, writes a null value.
  , dataValue :: a
  -- ^ The value that was read/will be written, of type 'ReadBuffer' or 'WriteBuffer'.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GStorable)

{- | An opaque pointer type for the @dpiDataBuffer@ union that we read from.
We cannot write to this in a way that ODPIC could use.
For poking purposes, use 'WriteBuffer'.
-}
newtype ReadBuffer = ReadBuffer (Ptr ReadBuffer)
  deriving (Show, Eq)
  deriving newtype (Storable)

{- | @dpiDataBuffer@ union that we can write to.
We cannot read from this without a hint as to what type of data it contains.
-}
data WriteBuffer
  = AsInt64 Int64
  | AsUInt64 Word64
  | AsDouble Double
  | AsString CString
  | AsBytes DPIBytes
  | AsTimestamp DPITimestamp
  | AsBoolean Int
  | AsNull
  deriving (Show, Eq, Generic)

instance Storable WriteBuffer where
  sizeOf _ = sizeOf (undefined :: DPITimestamp)

  alignment _ = 8

  peek = error "WriteBuffer: peek not supported!"

  poke ptr (AsInt64 intVal) = poke (castPtr ptr) intVal
  poke ptr (AsUInt64 word64Val) = poke (castPtr ptr) word64Val
  poke ptr (AsDouble doubleVal) = poke (castPtr ptr) doubleVal
  poke ptr (AsString cStringVal) = poke (castPtr ptr) cStringVal
  poke ptr (AsBytes dpiBytesVal) = poke (castPtr ptr) dpiBytesVal
  poke ptr (AsTimestamp dpiTimeStampVal) = poke (castPtr ptr) dpiTimeStampVal
  poke ptr (AsBoolean cbool) = poke (castPtr ptr) cbool
  poke ptr AsNull = poke (castPtr ptr) nullPtr

-- | Column position, starting with 1 for the first column.
newtype Column = Column {getColumn :: Word32}
  deriving newtype (Num, Enum, Show)

{- | The 1-tuple type or single-value "collection".
Structurally equivalent to 'Data.Functor.Identity.Identity'.
-}
newtype Only a = Only {fromOnly :: a}
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (Enum)

-- Opaque type for dpiVar reference
data ODPICVar

-- Opaque type for dpiData reference
data ODPICData

data ErrorInfo = ErrorInfo
  { errorInfoCode :: CInt
  , errorInfoOffset16 :: Word16
  , errorInfoMessage :: CString
  , errorInfoMessageLength :: CUInt
  , errorInfoEncoding :: CString
  , errorInfoFnName :: CString
  , errorInfoAction :: CString
  , errorInfoSqlState :: CString
  , errorInfoIsRecoverable :: CInt
  , errorInfoIsWarning :: CInt
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (GStorable)

data OracleError = OracleError
  { oracleErrorFnName :: String
  -- ^ The public ODPI-C function name which was called in which the error took place.
  , oracleErrorAction :: String
  -- ^ The internal action that was being performed when the error took place.
  , oracleErrorMessage :: String
  -- ^ The error message as a byte string.
  , oracleErrorSqlState :: String
  -- ^ The SQLSTATE associated with the error.
  , oracleErrorCode :: Int
  , -- , oracleErrorOffset16 :: Word16
    -- , oracleErrorMessageLength :: Int
    oracleErrorIsRecoverable :: Bool
  -- ^ A boolean value indicating if the error is recoverable.
  , oracleErrorIsWarning :: Bool
  -- ^ A boolean value indicating if the error information is for a warning returned
  -- by Oracle that does not prevent the request operation from proceeding.
  }
  deriving (Show, Eq, Typeable, Exception)
