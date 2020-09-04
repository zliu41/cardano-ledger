{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.EraParams
  ( Era,
    Era.Crypto,
    ValueType,
    ExtraBody,
  )
where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Data.Kind (Type)
import Data.Typeable (Typeable)
import qualified Cardano.Ledger.Val as Val
import Cardano.Ledger.Era as Era
import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
  )
import Cardano.Prelude (NFData (), NoUnexpectedThunks (..))

class
  ( EraBase e,
    CryptoClass.Crypto (Era.Crypto e),
    Typeable e,
    Val.Val (ValueType e),
    EraCns (ExtraBody e)
  ) =>
  Era e
  where
  type ValueType e :: Type
  type ExtraBody e :: Type

type EraCns t =
  (Eq t,
  Show t,
  Typeable t,
  NFData t,
  NoUnexpectedThunks t,
  ToCBOR t,
  FromCBOR t)
