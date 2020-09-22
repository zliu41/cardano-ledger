{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines core type families which we know to vary from era to
-- era.
--
-- Families in this module should be indexed on era.
--
-- It is intended for qualified import:
-- > import qualified Cardano.Ledger.Core as Core
module Cardano.Ledger.Core
  ( -- * Compactible
    Compactible (..),
    ValType (..)
  )
where

import Data.Kind (Type)
import Cardano.Binary (FromCBOR (..), ToCBOR (..), Annotator)
import Data.Typeable (Typeable)

class (Compactible (Value era)) => ValType era where
  type family Value era :: Type

-- | A value is something which quantifies a transaction output.


--------------------------------------------------------------------------------

-- * Compactible

--
-- Certain types may have a "presentation" form and a more compact
-- representation that allows for more efficient memory usage. In this case,
-- one should make instances of the 'Compactible' class for them.
--------------------------------------------------------------------------------

class Compactible a where
  data CompactForm a :: Type
  toCompact :: a -> CompactForm a
  fromCompact :: CompactForm a -> a


instance (Compactible a, ToCBOR a) => ToCBOR (CompactForm a) where
  toCBOR = toCBOR . fromCompact

instance (Compactible a, FromCBOR a) => FromCBOR (CompactForm a) where
  fromCBOR = toCompact <$> fromCBOR

instance (Typeable a, Compactible a, FromCBOR (Annotator a)) => FromCBOR (Annotator (CompactForm a)) where
  fromCBOR = (fmap . fmap) toCompact fromCBOR

