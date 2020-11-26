{-# LANGUAGE DeriveGeneric #-}

module Cardano.Ledger.Alonzo.TxBody
  ( IsFee (..),
    TxIn (..),
    TxOut (..),
    TxBody,
  )
where

import Cardano.Ledger.Alonzo.Data (DataHash)
import Cardano.Ledger.Alonzo.Scripts (ExUnits)
import Cardano.Ledger.Alonzo.TxWitness (ScriptDataHash)
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary.Value (Value)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval)
import Data.MemoBytes (MemoBytes)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.CompactAddr (CompactAddr)
import Shelley.Spec.Ledger.Delegation.Certificates (DCert)
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.TxBody (TxId, Wdrl)

-- | Tag indicating whether an input should be used to pay transaction fees.
-- This is used to prevent the entirety of a script's inputs being used for fees
-- in the case that the script fails to validate.
newtype IsFee = IsFee Bool
  deriving (Eq, Show, Typeable)

-- | Input of a UTxO. This references the transaction being spent from and the
-- index of the output being spent, as well as a tag indicating whether this
-- input should be used as a fee.
data TxIn era
  = TxInCompact
      {-# UNPACK #-} !(TxId era)
      {-# UNPACK #-} !Word64
      !IsFee
  deriving (Generic)

data TxOut era
  = TxOutCompact
      {-# UNPACK #-} !(CompactAddr era)
      !(CompactForm (Core.Value era))
      !(StrictMaybe (DataHash era))
  deriving (Generic)

data TxBodyRaw era = TxBodyRaw
  { inputs :: !(Set (TxIn era)),
    outputs :: !(StrictSeq (TxOut era)),
    certs :: !(StrictSeq (DCert era)),
    wdrls :: !(Wdrl era),
    txfee :: !Coin,
    vldt :: !ValidityInterval,
    update :: !(StrictMaybe (Update era)),
    mdHash :: !(StrictMaybe (MetaDataHash era)),
    mint :: !(Value era),
    exunits :: !ExUnits,
    scriptHash :: !(StrictMaybe (ScriptDataHash era))
  }
  deriving (Typeable)

newtype TxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
