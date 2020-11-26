{-# LANGUAGE DeriveGeneric #-}

module Cardano.Ledger.Alonzo.Scripts
  ( Tag (..),
    Script (..),
    ExUnits (..),
  )
where

import Cardano.Binary
import Cardano.Ledger.ShelleyMA.Timelocks
import Data.Coders
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Marker indicating the part of a transaction for which this script is acting
-- as a validator.
data Tag
  = -- | Validates spending a script-locked UTxO
    Input
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawl from a reward account
    Wdrl
  deriving (Eq, Generic, Ord, Show)

instance NoThunks Tag

data Script era
  = NativeScript (Timelock era)
  | NonNativeScript

-- | Arbitrary execution unit in which we measure the cost of scripts.
data ExUnits = ExUnits
  { exUnitsMem :: !Word64,
    exUnitsSteps :: !Word64
  }
  deriving (Eq, Show, Ord)

instance Semigroup ExUnits where
  ExUnits a c <> ExUnits b d = ExUnits (a + b) (c + d)

instance Monoid ExUnits where
  mempty = ExUnits 0 0

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance ToCBOR Tag where
  toCBOR = encode . encodeTag
    where
      encodeTag Input = Sum Input 0
      encodeTag Mint = Sum Mint 1
      encodeTag Cert = Sum Cert 2
      encodeTag Wdrl = Sum Wdrl 3

instance FromCBOR Tag where
  fromCBOR = decode $ Summands "Tag" decodeTag
    where
      decodeTag 0 = SumD Input
      decodeTag 1 = SumD Mint
      decodeTag 2 = SumD Cert
      decodeTag 3 = SumD Wdrl
      decodeTag n = Invalid n
