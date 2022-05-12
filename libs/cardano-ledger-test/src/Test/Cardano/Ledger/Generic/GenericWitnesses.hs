{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.GenericWitnesses
  ( witsVKeyNeeded',
    neededDataHashes,
    neededRedeemers,
    txOutLookupDatum,
    rdptrInv',
    neededInlineScripts,
    neededRefScripts,
  )
where

import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeededFromBody)
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as AlonzoScript
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose, alonzoInputHashes, rdptr)
import qualified Cardano.Ledger.Alonzo.Tx as AlonzoTx
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Tx (TxBody (inputs, outputs, referenceInputs), ValidatedTx (..))
import qualified Cardano.Ledger.Babbage.Tx as BabbageTx
import Cardano.Ledger.Babbage.TxBody (Datum (DatumHash), TxOut (TxOut))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.Core (Script)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Hashes (DataHash, ScriptHash)
import Cardano.Ledger.Keys (GenDelegs, KeyRole (Witness))
import Cardano.Ledger.Shelley.API (KeyHash, TxIn)
import Cardano.Ledger.Shelley.LedgerState
  ( WitHashes (..),
  )
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Shelley
import Cardano.Ledger.Shelley.UTxO (UTxO (UTxO), txins)
import qualified Data.Compact.SplitMap as SplitMap
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Maybe.Strict
  ( StrictMaybe (SJust, SNothing),
    strictMaybeToMaybe,
  )
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (getField))
import Test.Cardano.Ledger.Generic.Fields (TxField (Body), initialTx)
import Test.Cardano.Ledger.Generic.Proof (Proof (..))
import Test.Cardano.Ledger.Generic.Updaters (updateTx)

witsVKeyNeeded' ::
  forall era.
  Proof era ->
  UTxO era ->
  Core.TxBody era ->
  GenDelegs (Crypto era) ->
  Set (KeyHash 'Witness (Crypto era))
witsVKeyNeeded' proof utxo' txbody gd =
  unWitHashes $
    let tx = updateTx proof (initialTx proof) $ Body txbody
     in case proof of
          (Babbage _) -> Alonzo.witsVKeyNeeded utxo' tx gd
          (Alonzo _) -> Alonzo.witsVKeyNeeded utxo' tx gd
          (Shelley _) -> Shelley.witsVKeyNeeded utxo' tx gd
          (Mary _) -> Shelley.witsVKeyNeeded utxo' tx gd
          (Allegra _) -> Shelley.witsVKeyNeeded utxo' tx gd

neededDataHashes ::
  Proof era ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Core.TxBody era ->
  UTxO era ->
  Set (DataHash (Crypto era))
neededDataHashes proof m txbody utxo =
  let tx = updateTx proof (initialTx proof) $ Body txbody
   in case proof of
        (Babbage _) ->
          let spendinputs = inputs txbody
              UTxO mp = utxo
              smallUtxo = spendinputs SplitMap.◁ mp
              accum hashSet txout =
                case txout of
                  (TxOut _ _ (DatumHash dhash) _) -> Set.insert dhash hashSet
                  _ -> hashSet
              inputHashes = SplitMap.foldl' accum Set.empty smallUtxo
              outputDatumHashes = Set.fromList $ do
                (TxOut _ _ (DatumHash dh) _) <- toList $ outputs txbody
                return dh
           in inputHashes <> outputDatumHashes
        (Alonzo _) -> fst $ alonzoInputHashes m tx utxo
        _ -> mempty

neededRedeemers ::
  Proof era ->
  UTxO era ->
  Core.TxBody era ->
  [RdmrPtr]
neededRedeemers proof utxo txbody = case proof of
  Babbage _ -> catMaybes (tryGetRdmrPtr <$> scriptsNeededFromBody utxo txbody)
    where
      tryGetRdmrPtr = strictMaybeToMaybe . rdptr txbody . fst
  Alonzo _ -> catMaybes (tryGetRdmrPtr <$> scriptsNeededFromBody utxo txbody)
    where
      tryGetRdmrPtr = strictMaybeToMaybe . rdptr txbody . fst
  _ -> []

neededInlineScripts ::
  Proof era ->
  UTxO era ->
  Core.TxBody era ->
  [Script era]
neededInlineScripts (Babbage _) (UTxO utxom) txbody =
  do
    (Babbage.TxOut _ _ _ (SJust script@(AlonzoScript.TimelockScript _))) <-
      Map.elems $ Map.restrictKeys utxom (txins txbody)
    return script
neededInlineScripts _ _ _ = []

neededRefScripts ::
  Proof era ->
  UTxO era ->
  Core.TxBody era ->
  [Script era]
neededRefScripts (Babbage _) (UTxO utxo) txbody =
  do
    txin <- toList $ referenceInputs txbody
    case Map.lookup txin utxo of
      Just (Babbage.TxOut _ _ _ (SJust script)) -> return script
      Just _ -> []
      Nothing -> error "txout not found"
neededRefScripts _ _ _ = []

txOutLookupDatum :: Proof era -> Core.TxOut era -> Datum era
txOutLookupDatum (Babbage _) (Babbage.TxOut _ _ d _) = d
txOutLookupDatum (Alonzo _) (Alonzo.TxOut _ _ d) = case d of
  SJust d' -> Babbage.DatumHash d'
  SNothing -> Babbage.NoDatum
txOutLookupDatum (Allegra _) _ = Babbage.NoDatum
txOutLookupDatum (Mary _) _ = Babbage.NoDatum
txOutLookupDatum (Shelley _) _ = Babbage.NoDatum

rdptrInv' ::
  Proof era ->
  Core.TxBody era ->
  RdmrPtr ->
  StrictMaybe (ScriptPurpose (Crypto era))
rdptrInv' (Babbage _) txbody ptr = BabbageTx.rdptrInv txbody ptr
rdptrInv' (Alonzo _) txbody ptr = AlonzoTx.rdptrInv txbody ptr
rdptrInv' _ _ _ = SNothing
