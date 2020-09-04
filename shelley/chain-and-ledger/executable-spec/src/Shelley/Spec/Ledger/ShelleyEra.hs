{-# OPTIONS_GHC  -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE EmptyDataDecls   #-}
{-# LANGUAGE FlexibleInstances #-}

module Shelley.Spec.Ledger.ShelleyEra where


import Cardano.Ledger.EraRep
import Cardano.Ledger.Era
import Shelley.Spec.Ledger.TxData(Body(..),TxBodyShelley(..))
import Shelley.Spec.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Crypto as CLD

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

instance CLD.Crypto c => Era (Shelley c) where
  type Crypto (Shelley c) = c
  type ValueType (Shelley c) = Coin
  thisRep = Shelley

instance CLD.Crypto c => Body (Shelley c) where
    type instance TxBody (Shelley c) = TxBodyShelley (Shelley c)
    inputsB = _inputs
    outputsB = _outputs
    certsB = _certs
    wdrlsB = _wdrls
    txfeeB = Tag . _txfee
    ttlB = Tag . _ttl
    txUpdateB = _txUpdate
    bodyBytesB = undefined
    extraSizeB = Tag . extraSize
    mdHashB = _mdHash




-- ===============================================================================
