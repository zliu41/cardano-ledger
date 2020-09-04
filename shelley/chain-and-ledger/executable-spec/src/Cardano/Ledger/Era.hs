{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.Era
  ( Era,
    Crypto,
    Shelley,
    ShelleyMA
  )
where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Data.Kind (Type)
import Data.Typeable (Typeable)

class
  ( CryptoClass.Crypto (Crypto e),
    Typeable e
  ) =>
  Era e
  where
  type Crypto e :: Type

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

data Shelley c

instance CryptoClass.Crypto c => Era (Shelley c) where
  type Crypto (Shelley c) = c

--------------------------------------------------------------------------------
-- ShelleyMA Era
--------------------------------------------------------------------------------

data ShelleyMA c

instance CryptoClass.Crypto c => Era (ShelleyMA c) where
  type Crypto (ShelleyMA c) = c
