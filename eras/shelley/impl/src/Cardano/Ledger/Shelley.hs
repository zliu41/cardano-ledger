{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Definition of the shelley era, along with instances ot the @Core@ types
-- defined in @module Cardano.Ledger.Core@, and instances of the @API@ classes
-- exposed in @module Cardano.Ledger.Shelley.API@.
module Cardano.Ledger.Shelley
  ( ShelleyEra,
    Self,
    TxOut,
    TxBody,
    Value,
    Script,
    AuxiliaryData,
    PParams,
    Core.PParamsDelta,
    Tx,
    Witnesses,
    nativeMultiSigTag,
  )
where

import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (SupportsSegWit (..), ValidateScript (..))
import qualified Cardano.Ledger.Era as E (Era (..), TranslationContext)
import Cardano.Ledger.Shelley.BlockChain (bbHash)
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
  ( TxSeq (..),
    txSeqTxns,
  )
import Cardano.Ledger.Shelley.Constraints
  ( UsesPParams (..),
    UsesTxBody,
    UsesValue,
  )
import Cardano.Ledger.Shelley.Core (ShelleyEra)
import Cardano.Ledger.Shelley.Metadata (Metadata)
import Cardano.Ledger.Shelley.PParams (PParams, updatePParams)
import Cardano.Ledger.Shelley.Scripts (MultiSig)
import Cardano.Ledger.Shelley.Tx (WitnessSet, validateNativeMultiSigScript)
import qualified Cardano.Ledger.Shelley.Tx as STx (Tx, TxBody, TxOut (..))
import qualified Data.ByteString as BS

instance CryptoClass.Crypto c => UsesValue (ShelleyEra c)

instance CryptoClass.Crypto c => UsesPParams (ShelleyEra c) where
  mergePPUpdates _ = updatePParams

type instance E.TranslationContext (ShelleyEra c) = ()

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.PParams (ShelleyEra c) = PParams (ShelleyEra c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

-- | Magic number "memorialized" in the ValidateScript class under the method:
--   scriptPrefixTag:: Core.Script era -> Bs.ByteString, for the Shelley Era.
nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

instance
  (CryptoClass.Crypto c, UsesTxBody (ShelleyEra c)) =>
  ValidateScript (ShelleyEra c)
  where
  scriptPrefixTag _script = nativeMultiSigTag

  -- In the ShelleyEra there is only one kind of Script and its tag is "\x00"
  validateScript = validateNativeMultiSigScript

instance CryptoClass.Crypto c => SupportsSegWit (ShelleyEra c) where
  type TxSeq (ShelleyEra c) = Shelley.TxSeq (ShelleyEra c)
  fromTxSeq = Shelley.txSeqTxns
  toTxSeq = Shelley.TxSeq
  hashTxSeq = bbHash
  numSegComponents = 3

-- Self describing synonyms

type Value era = Coin

type Script era = MultiSig (E.Crypto era)

type AuxiliaryData era = Metadata era

type Self c = ShelleyEra c

type Tx era = STx.Tx era

type TxOut era = STx.TxOut era

type TxBody era = STx.TxBody era

type Witnesses era = WitnessSet (E.Crypto era)
