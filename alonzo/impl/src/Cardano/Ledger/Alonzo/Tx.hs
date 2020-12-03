-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Tx
  ( -- transaction
    Tx
      ( Tx,
        Tx',
        _body,
        _witness,
        _metadata,
        _isValidating,
        txFullBytes
      ),
    TxBody (..),
    TxOut (..),
    TxIn (..),
    TxId (..),
    decodeWits,
    segwitTx,
    txwitsScript,
    addrWits',
  )
where

import Cardano.Binary
  ( Annotator (..),
    Decoder,
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    annotatorSlice,
    decodeWord,
    encodeListLen,
    encodeMapLen,
    encodeNull,
    encodePreEncoded,
    encodeWord,
    serialize,
    serializeEncoding,
    withSlice,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import Cardano.Ledger.Shelley.Constraints (ShelleyBased, TxBodyConstraints)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.BaseTypes
  ( StrictMaybe,
    invalidKey,
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Hashing (EraIndependentTx, HashAnnotated (..))
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Serialization
  ( decodeList,
    decodeMapContents,
    decodeNullMaybe,
    decodeRecordNamed,
    encodeFoldable,
    encodeNullMaybe,
  )
import Shelley.Spec.Ledger.Tx
import Shelley.Spec.Ledger.TxBody
  ( TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    WitVKey (..),
    witKeyHash,
  )

-- ======
-- move this stuff eslewhere
-- | non-native script fee calculation
nonNativeScriptFees :: Tx era -> Coin
nonNativeScriptFees tx = prInit + prMem * (exUnitsMem (exunits $ _body tx)) + prSteps * (exUnitsSteps (exunits $ _body tx))

-- ======

-- | a tag (applied to alonzon transactions) to indicate whether all non-native scripts
-- carried by the transaction validate. Uses :
-- - to determine how to process the transaction (either in-full, or just take the fees)
-- - during tx re-application to avoid running all scripts
newtype IsValidating = IsValidating Bool
  deriving (Eq, Show, Typeable, ToCBOR, FromCBOR)

-- | A fully formed transaction.
data Tx era = Tx'
  { _body' :: !(Core.TxBody era),
    _witness' :: !(TxWitness era),
    _metadata' :: !(StrictMaybe (Core.Metadata era)),
    _isValidating' :: !IsValidating,
    txFullBytes :: BSL.ByteString
  }
  deriving (Generic)

deriving via
  AllowThunksIn
    '[ "txFullBytes"
     ]
    (Tx era)
  instance
    ShelleyBased era => NoThunks (Tx era)

deriving instance
  ShelleyBased era =>
  Show (Tx era)

deriving instance
  ShelleyBased era =>
  Eq (Tx era)

pattern Tx ::
  ( TxBodyConstraints era,
    ToCBOR (Core.Metadata era)
  ) =>
  Core.TxBody era ->
  TxWitness era ->
  StrictMaybe (Core.Metadata era) ->
  IsValidating ->
  Tx era
pattern Tx {_body, _witness, _metadata, _isValidating} <-
  Tx' _body _witness _metadata _isValidating _
  where
    Tx body witness metadata isv =
      let bodyBytes = serialize body
          wrappedMetadataBytes =
            serializeEncoding $
              encodeNullMaybe toCBOR (strictMaybeToMaybe metadata)
          fullBytes =
            (serializeEncoding $ encodeListLen 4)
              <> bodyBytes
              <> serialize witness
              <> wrappedMetadataBytes
              <> serialize isv
       in Tx'
            { _body' = body,
              _witness' = witness,
              _metadata' = metadata,
              _isValidating' = isv,
              txFullBytes = fullBytes
            }

{-# COMPLETE Tx #-}

instance ShelleyBased era => HashAnnotated (Tx era) era where
  type HashIndex (Tx era) = EraIndependentTx

segwitTx ::
  ( TxBodyConstraints era,
    ToCBOR (Core.Metadata era)
  ) =>
  Annotator (Core.TxBody era) ->
  Annotator (WitnessSet era) ->
  Maybe (Annotator (Core.Metadata era)) ->
  Annotator (IsValidating) ->
  Annotator (Tx era)
segwitTx
  bodyAnn
  witsAnn
  metaAnn
  isvAnn = Annotator $ \bytes ->
    let body = runAnnotator bodyAnn bytes
        witnessSet = runAnnotator witsAnn bytes
        metadata = flip runAnnotator bytes <$> metaAnn
        isv = runAnnotator isv bytes
        wrappedMetadataBytes = case metadata of
          Nothing -> serializeEncoding encodeNull
          Just b -> serialize b
        fullBytes =
          (serializeEncoding $ encodeListLen 4)
            <> serialize body
            <> serialize witnessSet
            <> wrappedMetadataBytes
            <> serialize isv
     in Tx'
          { _body' = body,
            _witnessSet' = witnessSet,
            _metadata' = maybeToStrictMaybe metadata,
            _isValidating' = isv
            txFullBytes = fullBytes
          }

instance
  ShelleyBased era =>
  ToCBOR (Tx era)
  where
  toCBOR tx = encodePreEncoded . BSL.toStrict $ txFullBytes tx

instance
  (ShelleyBased era, ValidateScript era) =>
  FromCBOR (Annotator (Tx era))
  where
  fromCBOR = annotatorSlice $
    decodeRecordNamed "Tx" (const 4) $ do
      body <- fromCBOR
      wits <- decodeWits
      meta <-
        ( decodeNullMaybe fromCBOR ::
            Decoder s (Maybe (Annotator (Core.Metadata era)))
          )
      isv <- fromCBOR
      pure $
        Annotator $ \fullBytes bytes ->
          Tx'
            { _body' = runAnnotator body fullBytes,
              _witnessSet' = runAnnotator wits fullBytes,
              _metadata' = (maybeToStrictMaybe $ flip runAnnotator fullBytes <$> meta),
              _isValidating' = runAnnotator isv fullBytes,
              txFullBytes = bytes
            }

-- | (All) script witness accessor function for Transactions
txwitsScript ::
  (TxBodyConstraints era, ToCBOR (Core.Metadata era)) =>
  Tx era ->
  Map (ScriptHash era) (Core.Script era)
txwitsScript tx = _scriptDataScripts' wts
  where
    Memo wts = (unScriptDataConstr $ _witness tx)
