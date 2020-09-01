{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Cardano.Ledger.EraDefinitions where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Shelley.Spec.Ledger.Value
import Shelley.Spec.Ledger.Val
import Shelley.Spec.Ledger.Coin
import Cardano.Ledger.Era

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

data Shelley c = Shc

shelleyEraTag :: Shelley c -> Int
shelleyEraTag = \_ -> 1

instance CryptoClass.Crypto c => Era (Shelley c) where
  type Crypto (Shelley c) = c
  type ValueType (Shelley c) = Coin
  eraTag = shelleyEraTag

type instance Forge (Shelley c) = ()


--------------------------------------------------------------------------------
-- ShelleyMA Era
--------------------------------------------------------------------------------

data ShelleyMA c = ShMAc

shelleyMAEraTag :: ShelleyMA c -> Int
shelleyMAEraTag = \_ -> 2

type instance Forge (ShelleyMA c) = Value (ShelleyMA c)

instance (CryptoClass.Crypto c) => (HasDefault (Value (ShelleyMA c))) where
  defaultV = vzero

instance CryptoClass.Crypto c => Era (ShelleyMA c) where
  type Crypto (ShelleyMA c) = c
  type ValueType (ShelleyMA c) = Value (ShelleyMA c)
  eraTag = shelleyMAEraTag
