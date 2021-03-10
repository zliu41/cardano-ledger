{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Pivo.Rules.Upec where

import Data.Typeable (Typeable)

import  Control.State.Transition (TRC (TRC), judgmentContext)
import qualified Control.State.Transition as T

import qualified Cardano.Ledger.Core as Core

import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)

import qualified Shelley.Spec.Ledger.STS.Epoch as Shelley (EPOCH, EpochPredicateFailure (UpecFailure))
import qualified Shelley.Spec.Ledger.LedgerState as Shelley

import qualified  Cardano.Ledger.Pivo.Update as Update

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
      TRC (_env, upecSt, ()) <- judgmentContext
      -- todo: here we should only change the protocol parameters on an epoch
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
