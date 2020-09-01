{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}



-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.Era
  ( Era,
    Crypto,
    ValueType,
    Forge,
    HasDefault,
    defaultV,
    eraTag,
  )
where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Data.Kind (Type)
import Data.Typeable (Typeable)
import qualified Shelley.Spec.Ledger.Val as ValClass
import Cardano.Binary
  ( ToCBOR (..),
    FromCBOR (..),
  )
import Cardano.Prelude
  ( NoUnexpectedThunks (..))

class
  ( CryptoClass.Crypto (Crypto e),
    Typeable e,
    ValClass.Val (ValueType e),
    FamConstraints (Forge e),
    HasDefault (Forge e)
  ) =>
  Era e
  where
  type Crypto e :: Type
  type ValueType e :: Type
  eraTag :: e -> Int

type family Forge e


type FamConstraints v = (FromCBOR v, ToCBOR v, Show v, Eq v, NoUnexpectedThunks v, Typeable v)

class HasDefault t where
  defaultV :: t

instance HasDefault () where
  defaultV = ()
