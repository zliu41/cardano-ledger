{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- The STS instance for UTXOW is technically an orphan.
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Pivo.Rules.Utxow where

import Shelley.Spec.Ledger.Credential (Credential (KeyHashObj))
import Shelley.Spec.Ledger.Address (Addr(Addr, AddrBootstrap), bootstrapKeyHash)
import Control.Monad.Trans.Reader (asks)
import Control.Monad (when)
import qualified Data.Sequence as Seq (filter)
import qualified Data.Sequence.Strict as StrictSeq
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash, ValidateAuxiliaryData (hashAuxiliaryData, validateAuxiliaryData))
import Cardano.Ledger.Core (ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary.Value (PolicyID, Value, policies, policyID)
import Cardano.Ledger.Shelley.Constraints (UsesAuxiliary, UsesScript, UsesTxBody, UsesTxOut, UsesValue)
import Cardano.Ledger.ShelleyMA.AuxiliaryData ()
import Cardano.Ledger.Pivo.Rules.Utxo (UTXO, UtxoPredicateFailure)
import Cardano.Ledger.Pivo.TxBody ()
import Cardano.Ledger.Torsor (Torsor (Delta))
import Control.SetAlgebra (eval, (◁), (∩))
import Control.State.Transition.Extended
import Data.Foldable (Foldable (toList))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Delegation.Certificates (requiresVKeyWitness, isInstantaneousRewards, delegCWitness, poolCWitness, genesisCWitness)
import Shelley.Spec.Ledger.Keys (DSignable, Hash, GenDelegs (GenDelegs), asWitness, GenDelegPair (genDelegKeyHash))
import Shelley.Spec.Ledger.LedgerState (UTxOState (_utxo), WitHashes(WitHashes)
                                       , witsFromWitnessSet, verifiedWits, diffWitHashes, nullWitHashes)
import qualified Shelley.Spec.Ledger.STS.Ledger as Shelley
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (UtxoEnv))
import Shelley.Spec.Ledger.STS.Utxow
  ( UtxowPredicateFailure (..)
  )
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Tx (Tx (Tx, _body), ValidateScript, hashScript, txwitsScript, validateScript, extractKeyHashWitnessSet)
import Shelley.Spec.Ledger.TxBody
  ( DCert (DCertPool, DCertDeleg, DCertGenesis),
    PoolCert (RegPool),
    PoolParams (_poolOwners),
    EraIndependentTxBody,
    RewardAcnt (getRwdCred),
    TxIn,
    Wdrl (unWdrl),
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO,
    txinLookup,
    getScriptHash,
    scriptCred,
    scriptStakeCred,
    txinsScript,
  )
import qualified Shelley.Spec.Ledger.SoftForks as SoftForks

import qualified Cardano.Ledger.Pivo.Update as Update

-- ==========================================================

-- | We want to reuse the same rules for Mary and Allegra. This however relies
-- on being able to get a set of 'PolicyID's from the value. Since a 'Coin' has
-- no policies, we create a small class which returns a null set of 'PolicyID's
-- for 'Coin'.
--
-- This should not escape this module.
class GetPolicies a crypto where
  getPolicies :: a -> Set (PolicyID crypto)

instance GetPolicies Coin crypto where
  getPolicies = const Set.empty

instance GetPolicies (Value crypto) crypto where
  getPolicies = policies

-- | Computes the set of script hashes required to unlock the transaction inputs
-- and the withdrawals.
scriptsNeeded ::
  ( UsesScript era,
    UsesTxOut era,
    UsesTxBody era,
    UsesAuxiliary era,
    GetPolicies (Core.Value era) (Crypto era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "mint" (Core.TxBody era) (Core.Value era)
  ) =>
  UTxO era ->
  Tx era ->
  Set (ScriptHash (Crypto era))
scriptsNeeded u tx =
  Set.fromList (Map.elems $ Map.mapMaybe (getScriptHash . (getField @"address")) u'')
    `Set.union` Set.fromList
      ( Maybe.mapMaybe (scriptCred . getRwdCred) $
          Map.keys withdrawals
      )
    `Set.union` Set.fromList
      ( Maybe.mapMaybe
          scriptStakeCred
          (filter requiresVKeyWitness certificates)
      )
    `Set.union` (policyID `Set.map` (getPolicies $ getField @"mint" txb))
  where
    txb = _body tx
    withdrawals = unWdrl $ getField @"wdrls" txb
    u'' = eval ((txinsScript (getField @"inputs" $ _body tx) u) ◁ u)
    certificates = (toList . getField @"certs") txb

--------------------------------------------------------------------------------
-- UTXOW STS
--------------------------------------------------------------------------------

data UTXOW era

instance
  forall era.
  ( UsesValue era,
    UsesTxBody era,
    UsesTxOut era,
    UsesAuxiliary era,
    UsesScript era,
    ChainData (Delta (Core.Value era)),
    SerialisableData (Delta (Core.Value era)),
    ValidateScript era,
    ValidateAuxiliaryData era,
    GetPolicies (Core.Value era) (Crypto era),
    Embed (Core.EraRule "UTXO" era) (UTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ Tx era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField
      "adHash"
      (Core.TxBody era)
      ( StrictMaybe
          (AuxiliaryDataHash (Crypto era))
      ),
    HasField "mint" (Core.TxBody era) (Core.Value era),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update.Payload era))
  ) =>
  STS (UTXOW era)
  where
  type State (UTXOW era) = UTxOState era
  type Signal (UTXOW era) = Tx era
  type Environment (UTXOW era) = UtxoEnv era
  type BaseM (UTXOW era) = ShelleyBase
  type
    PredicateFailure (UTXOW era) =
      UtxowPredicateFailure era
  transitionRules = -- [utxoWitnessed scriptsNeeded]
    [ do
        judgmentContext
          >>= \(TRC (UtxoEnv slot pp stakepools genDelegs, u, tx@(Tx txbody wits md))) -> do
            let utxo = _utxo u
            let witsKeyHashes = witsFromWitnessSet wits

            -- check scripts
            let failedScripts =
                  filter
                    ( \(hs, validator) ->
                        hashScript @era validator /= hs
                          || not (validateScript validator tx)
                    )
                    (Map.toList $ txwitsScript tx)
            case failedScripts of
              [] -> pure ()
              fs -> failBecause $ ScriptWitnessNotValidatingUTXOW $ Set.fromList $ fmap fst fs

            let sNeeded = scriptsNeeded utxo tx
                sReceived = Map.keysSet (txwitsScript tx)
            sNeeded == sReceived
              ?! MissingScriptWitnessesUTXOW
                (sNeeded `Set.difference` sReceived)

            -- check VKey witnesses
            verifiedWits tx ?!: InvalidWitnessesUTXOW

            let needed = witsVKeyNeeded utxo tx
                missingWitnesses = diffWitHashes needed witsKeyHashes
                haveNeededWitnesses = case nullWitHashes missingWitnesses of
                  True -> Right ()
                  False -> Left missingWitnesses
            haveNeededWitnesses ?!: MissingVKeyWitnessesUTXOW

            -- check metadata hash
            case (getField @"adHash" txbody, md) of
              (SNothing, SNothing) -> pure ()
              (SJust mdh, SNothing) -> failBecause $ MissingTxMetadata mdh
              (SNothing, SJust md') ->
                failBecause $
                  MissingTxBodyMetadataHash (hashAuxiliaryData @era md')
              (SJust mdh, SJust md') -> do
                hashAuxiliaryData @era md' == mdh ?! ConflictingMetadataHash mdh (hashAuxiliaryData @era md')
                -- check metadata value sizes
                when (SoftForks.validMetadata pp) $
                  validateAuxiliaryData @era md' ?! InvalidMetadata

            -- check genesis keys signatures for instantaneous rewards certificates
            let genDelegates =
                  Set.fromList $
                    fmap (asWitness . genDelegKeyHash) $
                      Map.elems genMapping
                (WitHashes khAsSet) = witsKeyHashes
                genSig = eval (genDelegates ∩ khAsSet)
                mirCerts =
                  StrictSeq.forceToStrict
                    . Seq.filter isInstantaneousRewards
                    . StrictSeq.fromStrict
                    $ getField @"certs" txbody
                GenDelegs genMapping = genDelegs

            coreNodeQuorum <- liftSTS $ asks quorum
            ( (not $ null mirCerts)
                ==> Set.size genSig >= fromIntegral coreNodeQuorum
              )
              ?! MIRInsufficientGenesisSigsUTXOW genSig

            trans @(Core.EraRule "UTXO" era) $
              TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)
    ]
    where
      witsVKeyNeeded utxo' (Tx txbody _ _) =
        WitHashes $
          certAuthors
            `Set.union` inputAuthors
            `Set.union` owners
            `Set.union` wdrlAuthors
            `Set.union` updateKeys
        where
          inputAuthors = foldr accum Set.empty (getField @"inputs" txbody)
            where
              accum txin ans =
                case txinLookup txin utxo' of
                  Just out ->
                    case getField @"address" out of
                      Addr _ (KeyHashObj pay) _ -> Set.insert (asWitness pay) ans
                      AddrBootstrap bootAddr ->
                        Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
                      _ -> ans
                  Nothing -> ans
          wdrlAuthors = Map.foldrWithKey accum Set.empty (unWdrl (getField @"wdrls" txbody))
            where
              accum key _ ans = Set.union (extractKeyHashWitnessSet [getRwdCred key]) ans
          owners = foldr accum Set.empty (getField @"certs" txbody)
            where
              accum (DCertPool (RegPool pool)) ans =
                Set.union
                  (Set.map asWitness (_poolOwners pool))
                  ans
              accum _cert ans = ans
          cwitness (DCertDeleg dc) = extractKeyHashWitnessSet [delegCWitness dc]
          cwitness (DCertPool pc) = extractKeyHashWitnessSet [poolCWitness pc]
          cwitness (DCertGenesis gc) = Set.singleton (asWitness $ genesisCWitness gc)
          cwitness c = error $ show c ++ " does not have a witness"
          -- key reg requires no witness but this is already filtered outby requiresVKeyWitness
          -- before the call to `cwitness`, so this error should never be reached.
          certAuthors = foldr accum Set.empty (getField @"certs" txbody)
            where
              accum cert ans | requiresVKeyWitness cert = Set.union (cwitness cert) ans
              accum _cert ans = ans
          updateKeys = fromSMaybe mempty
                     $ fmap Update.witnesses
                     $ getField @"update" txbody

  initialRules = []

instance
  ( Era era,
    STS (UTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ UtxoPredicateFailure era
  ) =>
  Embed (UTXO era) (UTXOW era)
  where
  wrapFailed = UtxoFailure

instance
  ( Era era,
    STS (UTXOW era),
    PredicateFailure (Core.EraRule "UTXOW" era) ~ UtxowPredicateFailure era
  ) =>
  Embed (UTXOW era) (Shelley.LEDGER era)
  where
  wrapFailed = Shelley.UtxowFailure
