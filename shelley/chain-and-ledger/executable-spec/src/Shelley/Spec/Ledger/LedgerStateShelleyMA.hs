{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.LedgerStateShelleyMA where

import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.UTxO
import Shelley.Spec.Ledger.TxData
import Cardano.Ledger.Era
import Cardano.Ledger.EraDefinitions
import Control.Iterate.SetAlgebra
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.Val

-- | Compute the lovelace which are destroyed by the ShelleyMA transaction
consumed ::
  forall era c. (Era era, era ~ (ShelleyMA c)) =>
  PParams ->
  UTxO era ->
  TxBody era ->
  ValueType era
consumed pp u tx =
  -- TODO does vplus work for subtraction
  vplus (vplus (_txforge tx) (balance (eval (txins tx ◁ u)))) (vinject $ refunds + withdrawals)
  where
    -- balance (UTxO (Map.restrictKeys v (txins tx))) + refunds + withdrawals + forge
    refunds = keyRefunds pp tx
    withdrawals = sum . unWdrl $ _wdrls tx
