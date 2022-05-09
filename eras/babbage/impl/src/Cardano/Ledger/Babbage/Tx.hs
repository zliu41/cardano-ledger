{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Cardano.Ledger.Babbage.Tx
  ( module X,
    TxBody (..),
    babbageInputHashes
  )
where

import Cardano.Ledger.Alonzo.Tx as X hiding (TxBody (..))
import Cardano.Ledger.Babbage.TxBody (TxBody (..), TxOut (TxOut), Datum (DatumHash))
import GHC.Records (HasField, getField)
import qualified Cardano.Ledger.Core as Core
import Data.Set (Set)
import Cardano.Ledger.Shelley.API (TxIn, ScriptHash, UTxO (UTxO))
import Cardano.Ledger.Era (ValidateScript, Crypto)
import qualified Data.Map as Map
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Set as Set

babbageInputHashes ::
  forall era.
  ( HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    ValidateScript era,
    Core.TxOut era ~ TxOut era
  ) =>
  Map.Map (ScriptHash (Crypto era)) (Core.Script era) ->
  ValidatedTx era ->
  UTxO era ->
  (Set (DataHash (Crypto era)), Set (TxIn (Crypto era)))
babbageInputHashes hashScriptMap tx (UTxO mp) = SplitMap.foldlWithKey' accum (Set.empty, Set.empty) smallUtxo
  where
    txbody = body tx
    spendinputs = getField @"inputs" txbody :: (Set (TxIn (Crypto era)))
    smallUtxo = spendinputs SplitMap.◁ mp
    accum ans@(hashSet, inputSet) txin txout =
      case txout of
        (TxOut addr _ (DatumHash dhash) _) ->
          if isTwoPhaseScriptAddressFromMap @era hashScriptMap addr
            then (Set.insert dhash hashSet, inputSet)
            else ans
        (TxOut addr _ _ _) ->
          if isTwoPhaseScriptAddressFromMap @era hashScriptMap addr
            then (hashSet, Set.insert txin inputSet)
            else ans
