{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Pivo.Rules.Pup where

import Data.Typeable (Typeable)

import  Control.State.Transition ()
import qualified Control.State.Transition as T

import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, StrictMaybe)

import qualified  Cardano.Ledger.Pivo.Update as Update

-- | Process update payload 🐶
data PUP era

instance Typeable era => T.STS (PUP era) where
  type Environment (PUP era) = Update.Environment era
  type State (PUP era) = Update.State era
  type Signal (PUP era) = StrictMaybe (Update.Payload era)
  type PredicateFailure (PUP era) = Update.PredicateFailure era

  type BaseM (PUP era) = ShelleyBase

  transitionRules = [error "Implement me!"]

  initialRules = []
