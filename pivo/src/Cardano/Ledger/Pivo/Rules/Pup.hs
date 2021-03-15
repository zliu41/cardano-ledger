{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Pivo.Rules.Pup where

import Data.Typeable (Typeable)
import Control.Monad (foldM)

import qualified Data.Text as Text

import  Control.State.Transition (TRC (TRC), judgmentContext)
import qualified Control.State.Transition as T

import qualified Cardano.Ledger.Update as UpdateAPI

import Cardano.Ledger.Era (Era)

import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, StrictMaybe (SNothing, SJust))

import Cardano.Ledger.Pivo.Update.Payload.SIP
  ( wrapSIPRevelation
  , wrapSIPSubmission
  , wrapSIPVote
  )
import Cardano.Ledger.Pivo.Update.Payload.Implementation
  ( wrapIMPRevelation
  , wrapIMPSubmission
  , wrapIMPVote
  )

import qualified Cardano.Ledger.Pivo.Update as Update

-- | Process update payload 🐶
data PUP era

instance (Typeable era, Era era) => T.STS (PUP era) where
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
          SJust p  -> do
            let
              sipSubmissions = wrapSIPSubmission <$> Update.sipSubmissions p
              sipRevelations = wrapSIPRevelation <$> Update.sipRevelations p
              sipVotes       = wrapSIPVote       <$> Update.sipVotes       p
              impSubmissions = wrapIMPSubmission <$> Update.impSubmissions p
              impRevelations = wrapIMPRevelation <$> Update.impRevelations p
              impVotes       = wrapIMPVote       <$> Update.impVotes       p
              -- todo: process the activation payload
            st' <- foldM (applyUpdate env)
                         (Update.unState st)
                         $  sipSubmissions
                         <> sipRevelations
                         <> sipVotes
                         <> impSubmissions
                         <> impRevelations
                         <> impVotes
            return $! Update.State st'
    ]
    where
      applyUpdate env st payload =
        case UpdateAPI.apply env payload st of
          Left err -> do
            T.failBecause $! Update.UpdateAPIFailure $ Text.pack $ show err
            return st
          Right st' -> return $! st'


  initialRules = []
