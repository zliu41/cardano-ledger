{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.LedgerStateShelley where

import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.UTxO
import Shelley.Spec.Ledger.TxData
import Cardano.Ledger.Era
import Cardano.Ledger.EraDefinitions
import Control.Iterate.SetAlgebra
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.Val

-- | Compute the lovelace which are destroyed by the Shelley transaction
consumed ::
  forall era c. (Era era, era ~ (Shelley c)) =>
  PParams ->
  UTxO era ->
  TxBody era ->
  ValueType era
consumed pp u tx =
  vplus (balance (eval (txins tx ◁ u))) (vinject $ refunds + withdrawals)
  where
    -- balance (UTxO (Map.restrictKeys v (txins tx))) + refunds + withdrawals
    refunds = keyRefunds pp tx
    withdrawals = sum . unWdrl $ _wdrls tx
