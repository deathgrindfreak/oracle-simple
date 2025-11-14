{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Avoid restricted function" -}

module Database.Oracle.Simple.Internal.DPIEnum
  ( CEnum (..),
    DPIEnum (..),
    CEnumChar (..),
    DPIEnumChar (..),
  )
where

import qualified Data.List as L
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Foreign.C.Types (CUChar (..), CUInt (..))
import Foreign.Ptr (castPtr)
import Foreign.Storable.Generic (Storable (..))

class (Enum a, Bounded a) => CEnum a where
  toCUInt :: a -> CUInt
  toCUInt = fromIntegral . fromEnum

  maybeFromCUInt :: CUInt -> Maybe a
  maybeFromCUInt i = L.find (\e -> toCUInt e == i) [minBound .. maxBound]

newtype DPIEnum a = DPIEnum a

instance (Typeable a, CEnum a) => Storable (DPIEnum a) where
  sizeOf _ = sizeOf (undefined :: CUInt)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    v <- peek (castPtr ptr)
    case maybeFromCUInt v of
      Nothing ->
        fail $
          (show . typeRep $ Proxy @a)
            <> ": Invalid value = "
            <> show v
      Just mode -> pure $ DPIEnum mode
  poke ptr (DPIEnum mode) =
    poke (castPtr ptr) (toCUInt mode)

class (Enum a, Bounded a) => CEnumChar a where
  toCUChar :: a -> CUChar
  toCUChar = fromIntegral . fromEnum

  maybeFromCUChar :: CUChar -> Maybe a
  maybeFromCUChar i = L.find (\e -> toCUChar e == i) [minBound .. maxBound]

newtype DPIEnumChar a = DPIEnumChar a

instance (Typeable a, CEnumChar a) => Storable (DPIEnumChar a) where
  sizeOf _ = sizeOf (undefined :: CUChar)
  alignment _ = alignment (undefined :: CUChar)
  peek ptr = do
    v <- peek (castPtr ptr)
    case maybeFromCUChar v of
      Nothing ->
        fail $
          (show . typeRep $ Proxy @a)
            <> ": Invalid value = "
            <> show v
      Just mode -> pure $ DPIEnumChar mode
  poke ptr (DPIEnumChar mode) =
    poke (castPtr ptr) (toCUChar mode)
