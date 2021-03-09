{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Pivo.Rules.Common where

import Control.Monad.Trans.Reader (asks, ReaderT)
import Data.Functor.Identity (Identity)

import Control.State.Transition.Extended (STS, Rule, BaseM, liftSTS)

import Cardano.Slotting.Slot (SlotNo (SlotNo))

import Shelley.Spec.Ledger.BaseTypes (Globals, epochInfo, stabilityWindow)
import Shelley.Spec.Ledger.Slot
  ( epochInfoEpoch
  , epochInfoFirst
  , epochInfoSize
  , unEpochSize
  )

import qualified  Cardano.Ledger.Pivo.Update as Update

-- | Elaborate the update environment for PPUP rule by querying the global
-- parameters.
getUpdateEnv
  :: ( STS sts
     , BaseM sts ~ ReaderT Globals Identity
     )
  => SlotNo
  -- ^ Current slot
  -> Rule sts ctx (Update.Environment era)
getUpdateEnv slot = liftSTS $
  do epInfo         <- asks epochInfo
     currentEpoch   <- epochInfoEpoch epInfo slot
     slotsPerEpoch  <- epochInfoSize epInfo currentEpoch
     epochFirstSlot <- epochInfoFirst epInfo currentEpoch
     stWindow       <- asks stabilityWindow
     return $!
       Update.Environment
         { Update.currentSlot = slot
         , Update.maxVotingPeriods = 1
           -- todo: we hardcode this for now. It seems this could be made
           -- part of the node configuration.
         , Update.slotsPerEpoch = SlotNo $ unEpochSize slotsPerEpoch
         , Update.epochFirstSlot = epochFirstSlot
         , Update.stabilityWindow = SlotNo stWindow
         }
