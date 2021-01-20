{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Pivo.Rules.Upec where

import Data.Typeable (Typeable)

import  Control.State.Transition ()
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

instance Typeable era => T.STS (UPEC era) where
  type Environment (UPEC era) = Shelley.EpochState era
  type State (UPEC era) = Shelley.UpecState era
  type Signal (UPEC era) = ()
  type PredicateFailure (UPEC era) = Update.PredicateFailure era

  type BaseM (UPEC era) = ShelleyBase

  transitionRules = [error "Implement me!"]

  initialRules = []

-- We need to guarantee that the Pivo UPEC rule can be embedded in the EPOCH
-- Shelley rule.
instance ( Typeable c
         , T.PredicateFailure (Core.EraRule "UPEC" c) ~ Update.PredicateFailure c
         ) => T.Embed (UPEC c) (Shelley.EPOCH c) where
  wrapFailed = Shelley.UpecFailure
