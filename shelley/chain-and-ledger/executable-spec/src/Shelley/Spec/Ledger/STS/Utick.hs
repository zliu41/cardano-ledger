{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Utick where

import Data.Typeable (Typeable)

import Control.State.Transition
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Slot (SlotNo)

import qualified Cardano.Ledger.Core as Core

data UTICK era

instance Typeable era => STS (UTICK era) where

  type State (UTICK era) = State (Core.EraRule "PPUP" era)
  type Signal (UTICK era) = SlotNo
  type Environment (UTICK era) = NewEpochState era
  type BaseM (UTICK era) = ShelleyBase
  type PredicateFailure (UTICK era) = ()

  initialRules = []

  transitionRules = [
    do
      TRC (_, st, _) <- judgmentContext
      return $! st
    ]

instance ( Typeable era
         , BaseM sts ~ ShelleyBase
         ) => Embed (UTICK era) sts where
  wrapFailed = error "The update slot tick rules should not fail"
