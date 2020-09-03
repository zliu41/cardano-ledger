-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.HasFunctions
  ( Era,
  consumedE,
    Era.Crypto
  )
where

import qualified Cardano.Ledger.Era as Era
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.UTxO
import Shelley.Spec.Ledger.TxData
import Shelley.Spec.Ledger.Coin

class (Era.Era e) => Era e where
  consumedE :: PParams -> UTxO e -> TxBody e -> Coin
