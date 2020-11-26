{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Data
  ( Data,
    EraIndependentData,
    DataHash (..),
    hashData,
  )
where

import Cardano.Binary (ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Crypto (HASH)
import Cardano.Ledger.Era (Crypto, Era)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Hashing (HashAnnotated (..))

-- | TODO this should be isomorphic to the plutus type
data Data era
  deriving (Eq, Ord)

-- | TODO appropriate serialisation
instance Era era => ToCBOR (Data era) where
  toCBOR = undefined

data EraIndependentData

newtype DataHash era
  = DataHash
      (Hash.Hash (HASH (Crypto era)) EraIndependentData)
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks)

instance Era era => HashAnnotated (Data era) era where
  type HashIndex (Data era) = EraIndependentData

hashData :: Era era => Data era -> DataHash era
hashData = DataHash . hashAnnotated
