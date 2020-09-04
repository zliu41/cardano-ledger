{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}


-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.EraFuncs
  ( Cardano.Ledger.EraFuncs.Era,
    consumed
  )
where

import Cardano.Ledger.EraParams as EraParams
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era as Era
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.UTxO
import Shelley.Spec.Ledger.TxBody
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.LedgerState


class (EraParams.Era e) => Era e where
  consumed ::
    PParams ->
    UTxO e ->
    TxBody e ->
    Coin -- temporary! Should be ValueType era when TxOut is parametrized

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------


instance CryptoClass.Crypto c => Cardano.Ledger.EraFuncs.Era (Era.Shelley c) where
  consumed = consumedShelley

--------------------------------------------------------------------------------
-- ShelleyMA Era
--------------------------------------------------------------------------------


instance CryptoClass.Crypto c => Cardano.Ledger.EraFuncs.Era (Era.ShelleyMA c) where
  consumed p u tb = (consumedShelley p u tb) <> c
    where
      EraParams.CoinForge c = _extraEraBody tb
