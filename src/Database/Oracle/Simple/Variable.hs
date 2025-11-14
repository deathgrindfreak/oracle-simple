{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Oracle.Simple.Variable
  ( Variable (..),
    VarBinder (..),
    ToDPIData (..),
    ToBinding (..),
    BindDef (..),
    Bindings,
    BindSizes,
    SomeBind (..),
    newVar,
    bindRows,
    (:.) (..),
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Ap (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (First (..), Max (..))
import Data.Semigroup.Foldable (foldMap1) -- TODO Can switch to Data.Foldable once on base 4.18
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Data.Time as Time
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Generics (C1, D1, Generic, Generically (..), K1 (..), M1 (..), Rep (..), S1, U1, (:*:) (..), (:+:) (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Foreign as UIO
import qualified UnliftIO.IORef as IORef

import Database.Oracle.Simple.Internal
  ( Connection,
    DPINativeType (..),
    DPIOracleType (..),
    DPIStmt,
    DPITimestamp (..),
    ODPICData,
    ODPICVar,
    Only (..),
    dpiConn_newVar,
    dpiStmt_bindByPos,
    dpiVar_release,
    dpiVar_setFromBytes,
    throwOracleError,
    toCUInt,
    utcTimeToDPITimestamp,
  )

{- | Determines how to transfer the Haskell type to the Oracle database.
These instances should be treated as base conversions, special cases will require
defining an instance for a newtype over one of these base types.
One potential case would be if you need to encode a String to a NVARCHAR column instead
of VARCHAR.
-}
class ToDPIData a where
  setDPIData :: Variable -> Int -> a -> IO ()
  getDPIOracleType :: Proxy a -> DPIOracleType
  getDPINativeType :: Proxy a -> DPINativeType
  getBytesSize :: a -> IO Int

instance ToDPIData Int where
  setDPIData var pos val = setDPIData @Int64 var pos (fromIntegral val)
  getDPIOracleType _ = getDPIOracleType (Proxy @Int64)
  getDPINativeType _ = getDPINativeType (Proxy @Int64)
  getBytesSize _ = pure 0

instance ToDPIData Int32 where
  setDPIData var pos val = setDPIData @Int64 var pos (fromIntegral val)
  getDPIOracleType _ = getDPIOracleType (Proxy @Int64)
  getDPINativeType _ = getDPINativeType (Proxy @Int64)
  getBytesSize _ = pure 0

instance ToDPIData Double where
  setDPIData (dataRef -> fptr) (fromIntegral -> pos) (UIO.CDouble -> val) =
    UIO.withForeignPtr fptr $ \ptr -> do
      setDoubleAt ptr pos val
  getDPIOracleType _ = DPI_ORACLE_TYPE_NATIVE_DOUBLE
  getDPINativeType _ = DPI_NATIVE_TYPE_DOUBLE
  getBytesSize _ = pure 0

instance ToDPIData Int64 where
  setDPIData (dataRef -> fptr) (fromIntegral -> pos) (fromIntegral -> val) =
    UIO.withForeignPtr fptr $ \ptr -> do
      setInt64At ptr pos val
  getDPIOracleType _ = DPI_ORACLE_TYPE_NUMBER
  getDPINativeType _ = DPI_NATIVE_TYPE_INT64
  getBytesSize _ = pure 0

instance ToDPIData Bool where
  setDPIData (dataRef -> fptr) (fromIntegral -> pos) (UIO.fromBool -> val) =
    UIO.withForeignPtr fptr $ \ptr -> do
      setBooleanAt ptr pos val
  getDPIOracleType _ = DPI_ORACLE_TYPE_BOOLEAN
  getDPINativeType _ = DPI_NATIVE_TYPE_BOOLEAN
  getBytesSize _ = pure 0

-- | You should be using Text if you care about performance.
instance ToDPIData String where
  setDPIData (varPtr -> fptr) (fromIntegral -> pos) val =
    UIO.withForeignPtr fptr $ \ptr -> do
      res <- UIO.withCStringLen val $ \(str, fromIntegral -> len) ->
        dpiVar_setFromBytes ptr pos str len
      throwOracleError res
  getDPIOracleType _ = DPI_ORACLE_TYPE_VARCHAR
  getDPINativeType _ = DPI_NATIVE_TYPE_BYTES
  getBytesSize str = UIO.withCStringLen str $ \(_, len) -> pure len

instance ToDPIData T.Text where
  setDPIData (varPtr -> fptr) (fromIntegral -> pos) val =
    UIO.withForeignPtr fptr $ \ptr -> do
      res <- TF.withCStringLen val $ \(str, fromIntegral -> len) ->
        dpiVar_setFromBytes ptr pos str len
      throwOracleError res
  getDPIOracleType _ = DPI_ORACLE_TYPE_VARCHAR
  getDPINativeType _ = DPI_NATIVE_TYPE_BYTES
  getBytesSize = pure . TF.lengthWord8

instance (ToDPIData a) => ToDPIData (Maybe a) where
  setDPIData var@(dataRef -> fptr) pos mVal =
    UIO.withForeignPtr fptr $ \ptr -> do
      case mVal of
        Just val -> setDPIData var pos val
        Nothing -> setIsNull ptr (fromIntegral pos)
  getDPIOracleType _ = getDPIOracleType (Proxy @a)
  getDPINativeType _ = getDPINativeType (Proxy @a)
  getBytesSize mVal =
    case mVal of
      Just val -> getBytesSize val
      Nothing -> pure 0

instance ToDPIData Time.UTCTime where
  getDPIOracleType _ = DPI_ORACLE_TYPE_TIMESTAMP
  getDPINativeType _ = DPI_NATIVE_TYPE_TIMESTAMP
  setDPIData (dataRef -> fptr) (fromIntegral -> pos) (utcTimeToDPITimestamp -> val) =
    UIO.withForeignPtr fptr $ \ptr -> do
      let DPITimestamp {..} = val
      setTimestampAt
        ptr
        pos
        year
        month
        day
        hour
        minute
        second
        fsecond
        tzHourOffset
        tzMinuteOffset
  getBytesSize = const $ pure 0

foreign import ccall "setDoubleAt"
  setDoubleAt :: UIO.Ptr ODPICData -> UIO.CUInt -> UIO.CDouble -> IO ()

foreign import ccall "setInt64At"
  setInt64At :: UIO.Ptr ODPICData -> UIO.CUInt -> UIO.CInt -> IO ()

foreign import ccall "setBooleanAt"
  setBooleanAt :: UIO.Ptr ODPICData -> UIO.CUInt -> UIO.CBool -> IO ()

foreign import ccall "setIsNull"
  setIsNull :: UIO.Ptr ODPICData -> UIO.CUInt -> IO ()

foreign import ccall "setTimestampAt"
  setTimestampAt ::
    UIO.Ptr ODPICData ->
    UIO.CUInt -> -- pos
    UIO.Int16 -> -- year
    UIO.Word8 -> -- month
    UIO.Word8 -> -- day
    UIO.Word8 -> -- hour
    UIO.Word8 -> -- minute
    UIO.Word8 -> -- second
    UIO.Word32 -> -- fsecond
    UIO.Int8 -> -- tzHourOffset
    UIO.Int8 -> -- tzMinuteOffset
    IO ()

-- The C library manages the memory of the variable and its data buffer.
-- We keep references to them so we can clean up after we're done using them.
data Variable = MkVariable
  { dataRef :: ForeignPtr ODPICData
  , varPtr :: ForeignPtr ODPICVar
  }

newVar ::
  (MonadUnliftIO m) =>
  Connection ->
  DPIOracleType ->
  DPINativeType ->
  Int ->
  Int ->
  m Variable
newVar fconn otyp ntyp maxArraySize size = do
  -- Extract actual pointers after ODPI-C allocates them
  (varPtr, bufPtr) <- UIO.withForeignPtr fconn $ \connPtr ->
    UIO.alloca $ \vPtrOut ->
      UIO.alloca $ \bufPtrOut -> liftIO $ do
        throwOracleError
          =<< dpiConn_newVar
            connPtr
            (toCUInt otyp)
            (toCUInt ntyp)
            (fromIntegral maxArraySize)
            (fromIntegral size)
            True
            False
            nullPtr
            vPtrOut
            bufPtrOut
        vPtr <- UIO.peek vPtrOut
        bPtr <- UIO.peek bufPtrOut
        pure (vPtr, bPtr)

  fvarPtr <- UIO.newForeignPtr dpiVar_release varPtr
  fbufPtr <- UIO.newForeignPtr_ bufPtr -- No finalizer for buffer
  pure $ MkVariable fbufPtr fvarPtr

-- | Variable (dpiVar) bindings for Haskell types.
data BindDef = MkBindDef
  { oracleType :: First DPIOracleType
  , nativeType :: First DPINativeType
  , maxSize :: Max Int
  , setters :: [Variable -> IO ()]
  }
  deriving stock (Generic)
  deriving (Semigroup) via Generically BindDef

newtype Bindings = MkBindings (IntMap BindDef)

instance Semigroup Bindings where
  (MkBindings a) <> (MkBindings b) = MkBindings $ IM.unionWith (<>) a b

instance Monoid Bindings where
  mempty = MkBindings IM.empty

{- | A composite type to bind custom data structures to statements.
For example:
@
query'
  "select * from test where num in (:1, :2) and txt = :3"
  ([100, 200] :. Only "hello")
@
-}
data h :. t = h :. t deriving (Eq, Ord, Show, Read)

infixr 3 :.

{- | Generically constructs a map of bind definitions for each left-to-right
positional column of a product type, usually a record. Intended to be used in a
kind of generic foldMap operation, where we collect all the bind variables into
an IntMap (keyed on column position).
-}
class ToBinding a where
  toBinding :: a -> VarBinder Bindings
  default toBinding :: (GToBinding (Rep a), Generic a) => a -> VarBinder Bindings
  toBinding = gToBinding . from

class GToBinding f where
  gToBinding :: f a -> VarBinder Bindings

instance GToBinding U1 where
  gToBinding _ = pure $ MkBindings IM.empty

instance (GToBinding m) => GToBinding (D1 i m) where
  gToBinding (M1 x) = gToBinding x

instance (GToBinding m) => GToBinding (C1 i m) where
  gToBinding (M1 x) = gToBinding x

instance (GToBinding m) => GToBinding (S1 i m) where
  gToBinding (M1 x) = gToBinding x

instance (GToBinding l, GToBinding r) => GToBinding (l :*: r) where
  gToBinding (l :*: r) = do
    bl <- gToBinding l
    br <- gToBinding r
    pure $ bl <> br

instance (() ~ TypeError ('Text "Sum types not supported")) => GToBinding (l :+: r) where
  gToBinding = error "Sum types not supported"

instance (ToDPIData a) => GToBinding (K1 i a) where
  gToBinding (K1 x) = toBindDef x

instance (ToDPIData a) => ToBinding (Only a)

instance ToDPIData a => ToBinding (Identity a)

instance (ToDPIData a, ToDPIData b) => ToBinding (a, b)

instance (ToDPIData a, ToDPIData b, ToDPIData c) => ToBinding (a, b, c)

instance (ToDPIData a, ToDPIData b, ToDPIData c, ToDPIData d) => ToBinding (a, b, c, d)

instance (ToDPIData a, ToDPIData b, ToDPIData c, ToDPIData d, ToDPIData e) => ToBinding (a, b, c, d, e)

instance (ToDPIData a, ToDPIData b, ToDPIData c, ToDPIData d, ToDPIData e, ToDPIData f) => ToBinding (a, b, c, d, e, f)

instance (ToDPIData a, ToDPIData b, ToDPIData c, ToDPIData d, ToDPIData e, ToDPIData f, ToDPIData g) => ToBinding (a, b, c, d, e, f, g)

instance (ToBinding a, ToBinding b) => ToBinding (a :. b) where
  toBinding (a :. b) = getAp $ mconcat [Ap $ toBinding a, Ap $ toBinding b]

{- | Useful when binding multiple homogenous params in a query.
It creates a variable for each parameter in the list and populates it with
its respective value. This is useful for dynamic params, i.e. when you can't construct a
product type for use with the default ToBinding instances.

IMPORTANT: You must ensure you have the correct amount of parameters in your statement
based on the length of the list.
-}
instance {-# OVERLAPPABLE #-} (ToDPIData a) => ToBinding [a] where
  toBinding vals = toBinding (MkSomeBind <$> vals)

data SomeBind where
  MkSomeBind :: ToDPIData a => a -> SomeBind

{- | Useful for binding dynamic parameters to a query when the list of parameters is heterogenous.

IMPORTANT: You must ensure you have the correct amount of parameters in your statement
based on the length of the list.
-}
instance {-# OVERLAPPING #-} ToBinding [SomeBind] where
  toBinding vals = do
    MkBindingState {col, row} <- ask
    newCol <- IORef.atomicModifyIORef' col (\c -> (c + length vals, c + 1))
    getAp $ flip foldMap (zip [newCol .. (newCol + length vals)] vals) $ \(col', MkSomeBind (val :: v)) -> do
      size <- Ap . liftIO $ getBytesSize val
      pure . MkBindings $
        IM.singleton
          col'
          MkBindDef
            { oracleType = First $ getDPIOracleType (Proxy @v)
            , nativeType = First $ getDPINativeType (Proxy @v)
            , maxSize = Max size
            , setters = pure $ \var -> setDPIData var row val
            }

data BindingState = MkBindingState
  { col :: IORef.IORef Int
  , row :: Int
  }

initBindingState :: MonadIO m => Int -> m BindingState
initBindingState row = do
  col <- IORef.newIORef 0
  pure MkBindingState {col, row}

newtype VarBinder a = VarBinder {runVarBinder :: ReaderT BindingState IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadUnliftIO
    , MonadReader BindingState
    )

toBindDef :: forall a. (ToDPIData a) => a -> VarBinder Bindings
toBindDef val = do
  MkBindingState {col, row} <- ask
  newCol <- IORef.atomicModifyIORef' col (\c -> (c + 1, c + 1))
  size <- liftIO $ getBytesSize val
  pure . MkBindings $
    IM.singleton
      newCol
      MkBindDef
        { oracleType = First $ getDPIOracleType (Proxy @a)
        , nativeType = First $ getDPINativeType (Proxy @a)
        , maxSize = Max size
        , setters = pure $ \var -> setDPIData var row val
        }

type BindSizes = IntMap Int

{- | Bind values to parameters in a statement from multiple rows using the arrays stored in bind
variables. Each row must be scanned to determine the maximum size in bytes that
is allocated for the bind variable (only needed for character and buffer
fields) -- this can potentially hurt performance if you are not using Text
fields (i.e. using String).
-}
bindRows ::
  ( ToBinding a
  , MonadUnliftIO m
  ) =>
  Connection ->
  Ptr DPIStmt ->
  NonEmpty a ->
  m Int
bindRows conn stmt rows = do
  let totalRows = length rows

  -- Get all of our bind variable inputs
  (MkBindings bindings) <- liftIO $
    getAp <$> flip foldMap1 (NE.zip (NE.fromList [0 ..]) rows) $ \(idx, row) ->
      Ap $ runReaderT (runVarBinder $ toBinding row) =<< initBindingState idx

  -- Create variables and bind them by column position to the statement.
  -- Finally populate the variable data by inserting each row's columns into
  -- their respective positions in the variable's data buffer.
  void $ flip IM.traverseWithKey bindings $ \column bDef -> do
    var <-
      newVar
        conn
        (getFirst $ oracleType bDef)
        (getFirst $ nativeType bDef)
        totalRows
        (getMax $ maxSize bDef)
    UIO.withForeignPtr (varPtr var) $ \vptr -> do
      liftIO $ dpiStmt_bindByPos stmt (fromIntegral column) vptr >>= throwOracleError
    liftIO $ traverse_ ($ var) (setters bDef)

  pure totalRows
