{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Pivo.Rules.Pup where

import Data.Typeable (Typeable)

import Control.Monad (foldM)

import  Control.State.Transition (TRC (TRC), judgmentContext)
import qualified Control.State.Transition as T

import Cardano.Ledger.Update.Proposal (Payload (Submit))

import qualified Cardano.Ledger.Update as Ledger.Update


import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, StrictMaybe (SNothing, SJust))

import Cardano.Ledger.Pivo.Update.Payload.SIP (wrapSubmission)

import qualified Cardano.Ledger.Pivo.Update as Update

-- | Process update payload 🐶
data PUP era

instance Typeable era => T.STS (PUP era) where
  type Environment (PUP era) = Update.Environment era
  type State (PUP era) = Update.State era
  type Signal (PUP era) = StrictMaybe (Update.Payload era)
  type PredicateFailure (PUP era) = Update.PredicateFailure era

  type BaseM (PUP era) = ShelleyBase

  transitionRules =
    [ do
        TRC (env, st, mUpdatePayload) <- judgmentContext
        case mUpdatePayload of
          SNothing -> return $! st
          SJust p  ->
            let
              res = foldM
                      (\st' sipSub -> Ledger.Update.apply env sipSub st')
                      (Update.unState st)
                      (fmap
                         (Ledger.Update.Ideation . Submit . wrapSubmission)
                         $ Update.sipSubmissions p
                      )
            in
            case res of
              Left err -> undefined
              Right st'' -> return $! Update.State st''
              -- TODO: Apply the other update payloads as well.

              -- TODO: We might should accumulate errors instead of aborting at
              -- the first error.
    ]

  initialRules = []
