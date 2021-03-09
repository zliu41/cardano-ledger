{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Pivo.Rules.Upec where

import Data.Typeable (Typeable)

import  Control.State.Transition (TRC (TRC), judgmentContext)
import qualified Control.State.Transition as T

import Cardano.Ledger.Update.Env.TracksSlotTime
  ( TracksSlotTime
  , currentSlot
  , slotsPerEpoch
  , epochFirstSlot
  , stableAfter
  )

import qualified Cardano.Ledger.Update as UpdateAPI
import qualified Cardano.Ledger.Update.Env.HasVotingPeriodsCap
import qualified Cardano.Ledger.Update.Env.TracksSlotTime

import qualified Cardano.Ledger.Core as Core

import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)

import qualified Shelley.Spec.Ledger.STS.Epoch as Shelley (EPOCH, EpochPredicateFailure (UpecFailure))
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.EpochBoundary as Shelley (Stake (unStake))

import qualified  Cardano.Ledger.Pivo.Update as Update
import Cardano.Ledger.Pivo.Rules.Common (getUpdateEnv)

import Cardano.Ledger.Era (Era, Crypto)

-- | Update epoch change
data UPEC era

data PredicateFailure = NoFailure
  deriving (Eq, Show)

instance
  ( Typeable era
  , Update.State era ~ T.State (Core.EraRule "PPUP" era)
  ) => T.STS (UPEC era) where
  type Environment (UPEC era) = Shelley.EpochState era
  type State (UPEC era) = Shelley.UpecState era
  type Signal (UPEC era) = ()
  type PredicateFailure (UPEC era) = Update.PredicateFailure era

  type BaseM (UPEC era) = ShelleyBase

  transitionRules = [
    do
      TRC (env, upecSt, ()) <- judgmentContext
      -- todo: the epoch change rule is not the appropriate place to call the
      -- tick rule. This should be done in some other place.
      --
      -- Here we should only change the protocol parameters if there is an epoch
      -- change.
      return $! upecSt
    ]

  initialRules = []

-- We need to guarantee that the Pivo UPEC rule can be embedded in the EPOCH
-- Shelley rule.
instance ( Typeable era
         , Update.State era ~ T.State (Core.EraRule "PPUP" era)
         , T.PredicateFailure (Core.EraRule "UPEC" era)
           ~ Update.PredicateFailure era
         ) => T.Embed (UPEC era) (Shelley.EPOCH era) where
  wrapFailed = Shelley.UpecFailure

-- | Auxiliary data structure we use to call UpdateAPI.tick
data TickEnv era =
  TickEnv
    { updateEnv :: Update.Environment era
    , sipVotersSD :: Shelley.Stake (Crypto era)
    }

instance TracksSlotTime (TickEnv era) where
  currentSlot = currentSlot . updateEnv

  slotsPerEpoch = slotsPerEpoch . updateEnv

  epochFirstSlot = epochFirstSlot . updateEnv

  stableAfter = stableAfter . updateEnv
