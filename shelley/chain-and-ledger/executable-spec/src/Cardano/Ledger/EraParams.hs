{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.EraParams
  ( Cardano.Ledger.EraParams.Era,
    Era.Crypto,
    ValueType,
    ExtraBody,
    dflt,
    HasDefault,
    CoinForge (..)
  )
where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Data.Kind (Type)
import Data.Typeable (Typeable)
import qualified Cardano.Ledger.Val as Val
import Cardano.Ledger.Era as Era
import Shelley.Spec.Ledger.Coin
import Cardano.Prelude

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
  )
import Cardano.Prelude (NFData (), NoUnexpectedThunks (..))

class
  ( Era.Era e,
    CryptoClass.Crypto (Era.Crypto e),
    Typeable e,
    Val.Val (ValueType e),
    EraCns (ExtraBody e),
    HasDefault (ExtraBody e)
  ) =>
  Era e
  where
  type ValueType e :: Type
  type ExtraBody e :: Type

class HasDefault t where
  dflt :: t

type EraCns t =
  (Eq t,
  Show t,
  Typeable t,
  NFData t,
  NoUnexpectedThunks t,
  ToCBOR t,
  FromCBOR t)

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------
instance HasDefault () where
  dflt = ()

instance CryptoClass.Crypto c => Cardano.Ledger.EraParams.Era (Shelley c) where
  type ValueType (Shelley c) = Coin
  type ExtraBody (Shelley c) = ()


--------------------------------------------------------------------------------
-- ShelleyMA Era
--------------------------------------------------------------------------------
instance HasDefault (CoinForge ) where
  dflt = CoinForge (Coin 1)

newtype CoinForge = CoinForge Coin
  deriving (Show, Eq, ToCBOR, FromCBOR, NFData, Generic, NoUnexpectedThunks)


instance CryptoClass.Crypto c => Cardano.Ledger.EraParams.Era (ShelleyMA c) where
  type ValueType (ShelleyMA c) = Coin
  type ExtraBody (ShelleyMA c) = CoinForge
