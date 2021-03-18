{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Pivo.Rules.Upec where

import Data.Typeable (Typeable)

import  Control.State.Transition (TRC (TRC), judgmentContext)
import qualified Control.State.Transition as T

import qualified Cardano.Ledger.Update as UpdateAPI

import Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Core as Core

import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.PParams (updatePParams)
import Shelley.Spec.Ledger.LedgerState (currentPp, ppupState)

import qualified Shelley.Spec.Ledger.STS.Epoch as Shelley (EPOCH, EpochPredicateFailure (UpecFailure))
import qualified Shelley.Spec.Ledger.LedgerState as Shelley

import qualified Cardano.Ledger.Pivo.Update as Update
import qualified Cardano.Ledger.Pivo.Update.Payload.Implementation as IMP

-- | Update epoch change
data UPEC era

data PredicateFailure = NoFailure
  deriving (Eq, Show)

instance
  ( Typeable era
  , Era era
  , Update.State era ~ T.State (Core.EraRule "PPUP" era)
  ) => T.STS (UPEC era) where
  type Environment (UPEC era) = Shelley.EpochState era
  type State (UPEC era) = Shelley.UpecState era
  type Signal (UPEC era) = ()
  type PredicateFailure (UPEC era) = Update.PredicateFailure era

  type BaseM (UPEC era) = ShelleyBase

  transitionRules = [
    do
      TRC (_env, upecSt, ()) <- judgmentContext
      let ppUpdate = IMP.impParametersUpdate $ UpdateAPI.getCurrentProtocol (ppupState upecSt)
      return $! upecSt { currentPp = currentPp upecSt `updatePParams` ppUpdate }
    ]

  initialRules = []

-- We need to guarantee that the Pivo UPEC rule can be embedded in the EPOCH
-- Shelley rule.
instance ( Typeable era
         , Era era
         , Update.State era ~ T.State (Core.EraRule "PPUP" era)
         , T.PredicateFailure (Core.EraRule "UPEC" era)
           ~ Update.PredicateFailure era
         ) => T.Embed (UPEC era) (Shelley.EPOCH era) where
  wrapFailed = Shelley.UpecFailure
